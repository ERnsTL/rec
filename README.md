# rec - Recfiles for Rust!

Incomplete implementation for handling awesome file format called [recfile](https://www.gnu.org/software/recutils/manual/recutils.html).

Supports parsing databases, sex'es, fields encryption and some basic queries. Also my first time writing parsers in Rust.

## Status

Basically useful. Not recommended for production use at the moment. This is currently the library only. The command-line utilities are still missing.

Tests for all examples mentioned in the manual are being added. During that, compatibility problems are revealed and fixed but a good part of the features are usable and compliant with GNU recutils.

API may change at any time at this stage.

## Compatibility

This library is compatible with the format and examples described in the [GNU recutils manual](https://www.gnu.org/software/recutils/manual/) for GNU recutils version 1.8, 3 January 2019. (When a new version is realeased, compare manual structure and diff for changes and pick up changes in tests, adjust implementation accordingly.)

The following examples and definitions are covered by unit tests, listed by chapter and section:

* 1 Introduction:  Nothing to comply with.
  * 1.1 Purpose:  Nothing to comply with.
  * 1.2 A Little Example:  Code example 1. TODO recsel example.
* 2 The Rec Format:  Nothing to comply with.
  * 2.1 Fields:  Field example, Field name regular expression, Field name case-sensitive, Valid field name examples, Value of a field, Escaping a newline (multi-line values).
  * 2.2 Records:  Record example, Several fields in a record share the same name and/or field value (multi-fields), Size of a record, Record separation.
  * 2.3 Comments:  Comment lines example, Comments example for headers and footers, Comments must be complete lines. TODO should comments also be saved again in the right place - how does GNU recutils handle this?
  * 2.4 Record Descriptors:  Nothing to comply with.
    * 2.4.1 Record Sets:  Recordset type example, Two record descriptors in the same database, Empty recordset example, Default record type (untyped) mixed with typed record types.
    * 2.4.2 Naming Record Types:  Allowed characters in record types.
    * 2.4.3 Documenting Records:  Recordset documentation field, Recordset documentation character restrictions, Two record sets with rec and doc fields example. TODO are multiple %doc per %rec allowed?
    * 2.4.4 Record Sets Properties:  Special fields example for mandatory, Special fields start with "%", Non-special fields in a record descriptor have no effect, Every recordset must contain exactly one special field rec, It is not mandated that the rec special field must occupy the first position, Special fields defined in the recutils format:  %rec, %mandatory, %allowed, %prohibit, %unique, %key, %doc, %typedef and %type, %auto, %sort, %size, %constraint, %confidential. Unknown special fields (TODO how does GNU recutils behave - warning or error?).
* 3 querying recfiles section TODO expand
* 4 editing records section TODO expand
* 5 editing fields section TODO expand
* 6 Field Types:  Nothing to comply with.
  * 6.1 Declaring Types:  TODO
  * 6.2 Types and Fields:  TODO
  * 6.3 Scalar Field Types:  TODO
  * 6.4 String Field Types:  TODO
  * 6.5 Enumerated Field Types:  TODO
  * 6.6 Date and Time Types:  TODO
  * 6.7 Other Field Types:  TODO
* 7 Constraints on Record Sets:  Nothing to comply with.
  * 7.1 Mandatory Fields:  TODO
  * 7.2 Prohibited Fields:  TODO
  * 7.3 Allowed Fields:  TODO
  * 7.4 Keys and Unique Fields:  TODO
  * 7.5 Size Constraints:  TODO
  * 7.6 Arbitrary Constraints:  TODO
* 8 Checking Recfiles:  TODO add recfix command
  * 8.1 Syntactical Errors:  TODO
  * 8.2 Semantic Errors:  TODO
* 9 Remote Descriptors:  TODO expand requirements - "any schema supported by libcurl", url reference, file reference
* 10 Grouping and Aggregates:  Nothing to comply with.
  * 10.1 Grouping Records:  TODO
  * 10.2 Aggregate Functions:  TODO
* 11 queries which join records:  TODO expand
* 12 auto-generated fields:  TODO expand
* 13 encryption:  TODO expand
* 14 generating imports:  TODO expand
* 15 interoperability:  TODO expand
* 17 invoking the utilities:  TODO cover features and switches for the utilities
* 18 regular expressions:  TODO expand
* 19 date input formats:  TODO expand

## Implementation notes

* The record descriptor special field "prohibit" is correct as per manual and GNU recutils source code, even though it is called "prohibited fields" in the manual and the special field for allowed fields is called "allowed". This is an inconsistency in the file format. It would be possible to allow %prohibited in this implementation, but it would error on GNU recutils.

## Future

* TODO compatibility and compliance with v1.9 from 2022
* TODO performance comparison GNU recutils with this implementation
* TODO equivalent of ndbmkhash for recutils - format of this hashfile? how does it detect hashfile and database being out of sync?
* TODO add torture tests of malformed inputs based on tests in GNU recutils
* TODO add web interface and web API like [csvbase](https://github.com/calpaterson/csvbase)

## Remarks

My hope is that some day this library will become part of bigger GUI program for managing your stuff with plain-text databases. Stale while im trying to figure out if writing complex GUI in Rust today is even possible.

Drop a message to `ouxya at pm dot me` if you want to explore on this topic together.
