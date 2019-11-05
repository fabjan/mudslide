# Mudslide

A slow moving monotonically increasing mass of data.

_Mudslide_ is like a directory tree with pocket dimensions.
You store sets of files in it, and retrieve sets of files from it.
Common data is shared between file sets.

It's also a playground for trying out different programming languages,
so there are several implementations in here. This is not an application
ready for use anywhere.

## Running

See the README in each subdirectory in this repository.

They all have their own language appropriate build tools, no mono repo
build magic here.

## Usage

When running, all implementations serve this HTTP API for storing and
retrieving files and file sets (described by "manifests"):

    PUT /api/files/:checksum
    GET /api/files/:checksum

    PUT /api/manifests/:checksum
    GET /api/manifests/:checksum
    GET /api/manifests/

A file is any sequence of bytes. A manifest is a text file describing a
set of stored files. See [#Protocol].

## Protocol

In _Mudslide_ there are files and manifests. Files are just binary data.
Manifests describe sets of named files. Files and manifests can only be added.
Nothing can be changed or deleted.

All checksums are 40 character lower case hexadecimal sha1.

### Files

Files when stored do not have a name other than their checksum. The only way
to name files is by writing a manifest (see below).

### Manifests

(Inspired by [Fossil]])

A Manifest defines a set of files that have been stored. A manifest contains a
list file checksums with filenames,  as well as a checksum for the manifest
itself. The manifest checksum is also the name of the manifest.

The manifest is utf-8 text. A manifest consists of one or more "cards"
separated by a single newline (ASCII: 0x0a) character. Each card begins with a
single character "card type". Zero or more arguments may follow the card type.
All arguments are separated from each other and from the card-type character
by a space character. There is no surplus white space between arguments
and no leading or trailing whitespace except for the newline character that
acts as the card separator. All cards must be in strict lexicographical order.
There may not be any duplicate cards. No card can contain newline characters.

    F file-checksum filename
    Z manifest-checksum

A manifest has one or more F cards. Each F card identifies a file that has
been stored. The card has two arguments. The first argument is the checksum
of the file content. The second argument is the pathname of the file.
No ".." or "." directories are allowed within the filename. Backslash
characters and newlines are not allowed within filenames.

A manifest must have a single Z card as its last line. The argument to the Z
card is the checksum of all prior lines of the manifest up to and including
the newline character that immediately precedes the "Z".

[Fossil]: https://www.fossil-scm.org/

## Pretend Backlog

- simple gossip protocol to let different servers cooperate storing and serving data
- more implementations: Go, Rust, C, Pony, Kotlin, Rebol ... the sky is the limit
- "manyfests", describing a set of manifests together
  - has `M checksum logical-name` cards and a `Z checksum` card only
  - not recursive, just one level of wrapping
- comments in manifests ("C cards")
  - would lead to possibly duplicate manifests with regards to file contents
- chunking files when storing
  - Park, KyoungSoo & Ihm, Sunghwan & Bowman, Mic & Pai, Vivek. (2007). Supporting Practical Content-Addressable Caching with CZIP Compression.
