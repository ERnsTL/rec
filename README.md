# rec
Incomplete implementation for handling awesome file format called [recfile](https://www.gnu.org/software/recutils/manual/recutils.html).
Supports parsing databases, sex'es, fields encryption and some basic queries. Also my first time writing parsers in Rust.

## Status

Very WIP.

## Features

Features missing compared to GNU recutils (TODO compare with as-is tests base):

* 2 rec format
  * multi-line fields
  * long line fields
  * multiple occurrences of a field per record
  * comments loading
  * comments saving
  * record descriptor
  * mix typed and non-typed record types
  * naming record types
  * document records
  * unique and key
  * doc
  * typedef and type
  * auto
  * sort
  * size
  * constraint
  * confidential
  * various date input formats
* 3 querying recfiles section TODO expand
* 4 editing records section TODO expand
* 5 editing fields section TODO expand
* 6 field types
  * anonymous fields, named fields
  * int field type
  * int range
  * int MIN, MAX
  * hexadecimal numbers
  * real numbers
  * size string in int, hexadecimal size, octal size
  * regexp string
  * enumerated field
  * enum comments
  * boolean field yes/no, 0/1, true/false
  * date field type, details in chapter 19
  * other: email
  * other: field
  * other: uuid
  * other: viz resp. foreign key
* 7 constraints on record sets
  * mandatory fields
  * prohibited fields
  * allowed fields
  * keys and unique fields
  * size constraint
  * arbitrary constraint
* 8 checking recfiles
  * syntactical errors
  * semantic errors
* 9 remote descriptors
  * "any schema supported by libcurl"
  * url reference
  * file reference
* 10 grouping and aggregates TODO expand
* 11 queries which join records TODO expand
* 12 auto-generated fields TODO expand
* 13 encryption TODO expand
* 14 generating imports TODO expand
* 15 interoperability TODE expand
* 17 invoking the utilities (TODO cover features)
* 18 regular expressions
* 19 date input formats

## Other

My hope is that some day this library will become part of bigger GUI program for managing your stuff with plain-text databases. Stale while im trying to figure out if writing complex GUI in Rust today is even possible.

Drop a message to `ouxya at pm dot me` if you want to explore on this topic together.