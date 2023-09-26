# rec

Incomplete implementation for handling awesome file format called [recfile](https://www.gnu.org/software/recutils/manual/recutils.html).

Supports parsing databases, sex'es, fields encryption and some basic queries. Also my first time writing parsers in Rust.

## Status

Very WIP.

## Compatibility

This library is compatible with the format and examples described in the [GNU recutils manual](https://www.gnu.org/software/recutils/manual/) for GNU recutils version 1.8, 3 January 2019. (When a new version is realeased, compare manual structure and diff for changes and pick up changes in tests, adjust implementation accordingly.)

The following examples and definitions are covered by unit tests, listed by chapter and section:

* 1 Introduction:  Nothing to comply with.
  * 1.1 Purpose:  Nothing to comply with.
  * 1.2 A Little Example:  Code example 1. TODO recsel example.
* 2 The Rec Format:  Nothing to comply with.
  * 2.1 Fields:  TODO Field example, TODO Field name regular expression, TODO Field name case-sensitive, TODO Valid field name examples, TODO Value of a field, TODO Escaping a newline (multi-line values).
  * 2.2 Records:  TODO Record example, TODO Several fields in a record share the same name and/or field value (multi-fields), TODO size of a record, TODO Record separation.
  * 2.3 Comments:  TODO Comment lines example, TODO Comments example for headers and footers, TODO Comments must be complete lines. TODO should comments also be saved again in the right place - how does GNU recfiles handle this?
  * 2.4 Record Descriptors:  Nothing to comply with.
    * 2.4.1 Record Sets:  TODO Recordset type example, TODO Two record descriptors in the same database, TODO empty recordset example, TODO Default record type (untyped) mixed with typed record types.
    * 2.4.2 Naming Record Types:  TODO Allowed characters in record types.
    * 2.4.3 Documenting Records:  TODO Set documentation, TODO Set documentation character restrictions, TODO Two record sets with rec and doc fields example.
    * 2.4.4 Record Sets Properties:  TODO Special fields example for mandatory, TODO Special fields start with "%", TODO Non-special fields in a record descriptor, TODO Every recordset must contain exactly one field rec, TODO It is not mandated that the rec field must occupy the first position, TODO Test for each special field:  TODO %rec, TODO %mandatory, TODO %allowed, TODO %prohibit, TODO %unique, TODO %key, TODO %doc, TODO %typedef and TODO %type, TODO %auto, TODO %sort, TODO %size, TODO %constraint, TODO %confidential, TODO unknown ones (warning?).
* 3 querying recfiles section TODO expand
* 4 editing records section TODO expand
* 5 editing fields section TODO expand
* 6 field types:  TODO use actual chapter numbering from manual
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
* 7 constraints on record sets:  TODO use actual chapter numbering from manual
  * mandatory fields
  * prohibited fields
  * allowed fields
  * keys and unique fields
  * size constraint
  * arbitrary constraint
* 8 checking recfiles:  TODO use actual chapter numbering from manual
  * syntactical errors
  * semantic errors
* 9 remote descriptors:  TODO use actual chapter numbering from manual
  * "any schema supported by libcurl"
  * url reference
  * file reference
* 10 grouping and aggregates:  TODO expand
* 11 queries which join records:  TODO expand
* 12 auto-generated fields:  TODO expand
* 13 encryption:  TODO expand
* 14 generating imports:  TODO expand
* 15 interoperability:  TODO expand
* 17 invoking the utilities:  TODO cover features and switches for the utilities
* 18 regular expressions:  TODO expand
* 19 date input formats:  TODO expand

## Remarks

My hope is that some day this library will become part of bigger GUI program for managing your stuff with plain-text databases. Stale while im trying to figure out if writing complex GUI in Rust today is even possible.

Drop a message to `ouxya at pm dot me` if you want to explore on this topic together.