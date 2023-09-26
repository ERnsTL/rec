# rec

Incomplete implementation for handling awesome file format called [recfile](https://www.gnu.org/software/recutils/manual/recutils.html).

Supports parsing databases, sex'es, fields encryption and some basic queries. Also my first time writing parsers in Rust.

## Status

Very WIP.

## Compatibility

This library is compatible with the format and examples described in the [GNU recutils manual](https://www.gnu.org/software/recutils/manual/) for GNU recutils version 1.8, 3 January 2019. (When a new version is realeased, compare manual structure and diff for changes and pick up changes in tests, adjust implementation accordingly.)

The following examples and definitions are covered by unit tests, listed by chapter and section:

* 1 Introduction: Nothing to comply with.
  * 1.1 Purpose: Nothing to comply with.
  * 1.2 A Little Example:  Code example 1. TODO cmdline example recsel.
* 2 The Rec Format:
  * 2.1 Fields
  * 2.2 Records
  * 2.3 Comments
  * 2.4 Record Descriptors
    * 2.4.1 Record Sets
    * 2.4.2 Naming Record Types
    * 2.4.3 Documenting Records
    * 2.4.4 Record Sets Properties
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

## Remarks

My hope is that some day this library will become part of bigger GUI program for managing your stuff with plain-text databases. Stale while im trying to figure out if writing complex GUI in Rust today is even possible.

Drop a message to `ouxya at pm dot me` if you want to explore on this topic together.