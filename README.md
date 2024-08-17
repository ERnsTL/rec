# rec - Recfiles for Rust!

Implementation for handling the awesome file format called [recfile](https://www.gnu.org/software/recutils/manual/recutils.html).

## Status

Basically useful. Not recommended for production use at the moment. Some features are still missing - it supports parsing databases, sex'es, fields encryption and some basic queries.

This is currently the library only. The command-line utilities are still missing. A server resp. daemon allowing multi-user online queries without re-parsing the database for every request is still missing.

Tests for all examples mentioned in the manual are currently being added. During that, compatibility problems are revealed and fixed as well as missing features added, but a good part of the features are usable and compliant with GNU recutils.

API may change at any time at this stage.

Algorithms and data types not yet optimized for performance. Internal structs could need a refactoring. The parser is not based on a parser framework like Nom nor a parser generator, but hand-written.

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
* 3 Querying Recfiles:  Nothing to comply with.
  * 3.1 Simple Selections:  TODO
  * 3.2 Selecting by Type:  TODO
  * 3.3 Selecting by Position:  TODO
  * 3.4 Random Records:  TODO
  * 3.5 Selection Expressions:  TODO check for compliance requirements in section front matter.
    * 3.5.1 Selecting by predicate:  TODO
    * 3.5.2 SEX Operands:  TODO
    * 3.5.3 SEX Operators:  TODO
    * 3.5.4 SEX Evaluation:  TODO
  * 3.6 Field Expressions:  TODO
  * 3.7 Sorted Output:  TODO
* 4 Editing Records: TODO check for compliance requirements in chapter front matter.
  * 4.1 Inserting Records:  TODO check for compliance requirements in section front matter.
    * 4.1.1 Adding Records With recins:  TODO
    * 4.1.2 Replacing Records With recins:  TODO
    * 4.1.3 Adding Anonymous Records:  TODO
  * 4.2 Deleting Records:  TODO
  * 4.3 Sorting Records:  TODO
* 5 Editing Fields:  TODO check for compliance requirements in chapter front matter.
  * 5.1 Adding Fields:  TODO
  * 5.2 Setting Fields:  TODO
  * 5.3 Deleting Fields:  TODO
  * 5.4 Renaming Fields:  TODO
* 6 Field Types:  Nothing to comply with.
  * 6.1 Declaring Types:  The typedef syntax, Define Age_t as numbers in the range 0 to 120, Type names are identifiers having the following syntax, A type can be declared to be an alias for another type, The order of the %typedef fields is not relevant, a type definition can forward-reference another type, complain if undefined types are referenced, complain if any aliases referencing in loop directly or indirectly in type declarations, The scope of a type is the record descriptor where it is defined.
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
* 11 Queries which Join Records:  TODO check for compliance requirements in chapter front matter.
  * 11.1 Foreign Keys:  TODO
  * 11.2 Joining Records:  TODO
* 12 auto-generated fields:  TODO check for compliance requirements in chapter front matter.
  * 12.1 Counters:  TODO
  * 12.2 Unique Identifiers:  TODO
  * 12.3 Time-Stamps:  TODO
* 13 encryption:  TODO check for compliance requirements in chapter front matter.
  * 13.1 Confidential Fields:  TODO
  * 13.2 Encrypting Files:  TODO
  * 13.3 Decrypting Data:  TODO
* 14 Generating Reports:  TODO check for compliance requirements in chapter front matter.
  * 14.1 Templates:  TODO
* 15 Interoperability:  Nothing to comply with.
  * 15.1 CSV Files:  TODO
  * 15.2 Importing MDB Files:  TODO
* 16 Bash Builtins:  TODO check for compliance requirements in chapter front matter.
  * 16.1 readrec:  TODO
* 17 Invoking the Utilities:  TODO check for compliance requirements in chapter front matter (common options).
  * 17.1 Invoking recinf:  TODO
  * 17.2 Invoking recsel:  TODO
  * 17.3 Invoking recins:  TODO
  * 17.4 Invoking recdel:  TODO
  * 17.5 Invoking recset:  TODO
  * 17.6 Invoking recfix:  TODO
  * 17.7 Invoking recfmt:  TODO
  * 17.8 Invoking csv2rec:  TODO
  * 17.9 Invoking rec2csv:  TODO
  * 17.10 Invoking mdb2rec:  TODO
* 18 Regular Expressions:  TODO
* 19 Date input formats:  TODO check for compliance requirements in chapter front matter.
  * 19.1 General date syntax:  TODO
  * 19.2 Calendar date items:  TODO
  * 19.3 Time of day items:  TODO
  * 19.4 Time zone items:  TODO
  * 19.5 Combined date and time of day items:  TODO
  * 19.6 Day of week items:  TODO
  * 19.7 Relative items in date strings:  TODO
  * 19.8 Pure numbers in date strings:  TODO
  * 19.9 Seconds since the Epoch:  TODO
  * 19.10 Specifying time zone rules:  TODO
  * 19.11 Authors of parse_datetime:  TODO

## Implementation notes

* The record descriptor special field "prohibit" is correct as per manual and GNU recutils source code, even though it is called "prohibited fields" in the manual and the special field for allowed fields is called "allowed". This is an inconsistency in the file format. It would be possible to allow %prohibited in this implementation, but it would error on GNU recutils (TODO check).

## Feedbacks on the manual

* 2.4.4 has inconsistent ordering in the listing of %mandatory, %allowed and %prohibited in the blockquote-styled text vs. ordering of the links in the description.
* 3.5.1 sub-section name is inconsistently cased
* 19 has non-uppercase name

## Future

* TODO compatibility and compliance with v1.9 from 2022
* TODO performance comparison GNU recutils with this implementation
* TODO equivalent of ndbmkhash for recutils - format of this hashfile? how does it detect hashfile and database being out of sync?
* TODO add torture tests of malformed inputs based on tests in GNU recutils
* TODO add web interface and web API like [csvbase](https://github.com/calpaterson/csvbase)
* TODO server resp. daemon allowing multi-user online queries without re-parsing the database for every request
