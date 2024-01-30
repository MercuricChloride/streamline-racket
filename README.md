# Streamline 🏊 

_THIS LANGUAGE IS IN DEVELOPMENT. THINGS MIGHT CHANGE OR BREAK UNTIL WE CUT A STABLE RELEASE_

A delightfully simple declarative, data driven programming language built for the EVM, specifically for developing and using Substreams in a more intuitive and powerful manner.

Check out the docs [HERE](doc/intro.md) to get started!

## Usage

``` sh
streamline [cmd] [opts]
```

## Examples

### Install streamline and it's dependencies

``` sh
streamline build <PATH_TO_FILE>
```

### Compile a streamline file

``` sh
streamline build <PATH_TO_FILE>
```

### Run a module from the last streamline file

``` sh
streamline run <MODULE_NAME>
```

### Run a repl

``` sh
streamline repl <MODULE_NAME>
```

### Add a solidity file to your global library

``` sh
streamline add <PATH_TO_FILE> <NAME_OF_LIBRARY_ENTRY>
```


### Bugs / Features in progress

I'm sure there are lots of bugs waiting to be found. If you find something please let me know!

#### Features ready by ~~11/27/2023~~ 11/30/2023
1. Modules w/ Param inputs
2. Sink Configs

_I pushed back the release of some of these features, as I spent a few days cleaning up and optimizing the code generation pathway. It makes building the repl much simpler. So that is only a few days of work away._

#### Features coming soon
1. Arbitrary substeams spkg interop
2. Rust fn interop

## License

Copyright © 2023 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
