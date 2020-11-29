use super::Err;
use aes::Aes128;
use block_modes::block_padding::ZeroPadding;
use block_modes::{BlockMode, Cbc};
use crc::crc32;
use rand_core::{OsRng, RngCore};
use std::convert::TryInto;
use std::error;
use std::fmt;
use std::str;

// Cipher used in recutils
type AesCbc = Cbc<Aes128, ZeroPadding>;

// Don't ask me about this
const IV_PADDING: &[u8] = &[4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
pub const ENCRYPTED_PREFIX: &str = "encrypted-";

#[derive(Debug, PartialEq)]
pub enum DecryptionError {
    InvalidPassword,
    MissingPrefix,
    MalformedData,
}

impl fmt::Display for DecryptionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use DecryptionError::*;

        match self {
            InvalidPassword => write!(f, "invalid password"),
            MissingPrefix => write!(f, "encrypted fields should start with encrypted- prefix"),
            MalformedData => write!(f, "malformed data"),
        }
    }
}

impl error::Error for DecryptionError {}

pub fn decrypt_field(pwd: &str, field: &str) -> Result<String, DecryptionError> {
    use DecryptionError::*;

    let stripped = field.strip_prefix(ENCRYPTED_PREFIX).ok_or(MissingPrefix)?;
    let data = base64::decode(stripped).map_err(|_| MalformedData)?;

    // message size should be at least one block + salt
    if data.len() < 20 {
        Err(MalformedData)?
    }

    let key = transform_key(pwd);
    let result = decrypt(&key, &data).map_err(|_| MalformedData)?;
    let (plaintext, crc) = result.split_at(result.len() - 4);

    if crc32::checksum_ieee(plaintext) != u32::from_le_bytes(crc.try_into().unwrap()) {
        // we decrypted some junk
        Err(InvalidPassword)?
    }

    Ok(str::from_utf8(plaintext)
        .map_err(|_| MalformedData)?
        .to_owned())
}

fn decrypt(key: &[u8], data: &[u8]) -> Result<Vec<u8>, Err> {
    let (ciphertext, salt) = data.split_at(data.len() - 4);

    let iv = [salt, IV_PADDING].concat();

    let cipher = AesCbc::new_var(key, &iv)?;

    Ok(cipher.decrypt_vec(ciphertext)?)
}

pub fn encrypt_field(pwd: &str, data: &str) -> String {
    // compute checksum
    let checksum = crc32::checksum_ieee(data.as_bytes());
    let data = [data.as_bytes(), &checksum.to_le_bytes()].concat();

    let encrypted = encrypt(&transform_key(pwd), &data);

    let mut encoded = base64::encode(encrypted);
    encoded.insert_str(0, ENCRYPTED_PREFIX);

    encoded
}

fn encrypt(key: &[u8], data: &[u8]) -> Vec<u8> {
    // generate random salt
    let mut salt = [0; 4];
    OsRng.fill_bytes(&mut salt);

    let iv: &[u8] = &[&salt[..], IV_PADDING].concat();

    let cipher = AesCbc::new_var(key, iv).expect("initialize cipher");

    let mut ciphertext = cipher.encrypt_vec(data);
    for i in 0..4 {
        ciphertext.push(salt[i])
    }

    ciphertext
}

fn transform_key(pwd: &str) -> [u8; 16] {
    // transform key
    let mut key = [0; 16];
    for i in 0..16 {
        key[i] = pwd.as_bytes()[i % pwd.len()]
    }

    key
}

#[cfg(test)]
mod tests {
    use super::*;

    // generated by original C implementation
    const ENCRYPTED_FIELD: &str = "encrypted-XR5T5qB2QRrfbafSvgFHos5IZCA=";
    const PLAINTEXT: &str = "secret";
    const PWD: &str = "unguessable";

    #[test]
    fn decrypt() {
        assert_eq!(
            decrypt_field(PWD, ENCRYPTED_FIELD).unwrap(),
            PLAINTEXT.to_owned()
        );

        assert_eq!(
            decrypt_field(PWD, "blah").unwrap_err(),
            DecryptionError::MissingPrefix
        );
        assert_eq!(
            decrypt_field(PWD, "encrypted-blah").unwrap_err(),
            DecryptionError::MalformedData
        );
        assert_eq!(
            decrypt_field("invalid", ENCRYPTED_FIELD).unwrap_err(),
            DecryptionError::InvalidPassword
        );
    }

    #[test]
    fn encrypt() {
        let encrypted = encrypt_field(PWD, PLAINTEXT);
        assert_eq!(encrypted.len(), ENCRYPTED_FIELD.len());
        assert_eq!(decrypt_field(PWD, &encrypted).unwrap(), PLAINTEXT);
    }
}
