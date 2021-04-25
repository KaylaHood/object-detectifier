# object-detectifier

Developed using GHC v8.8.2 and Cabal v3.2.0.0

Build instructions:
* In stack.yaml, change `extra-include-dirs` and `extra-lib-dirs` to point to your machine's sqlite3 installation and source folders.
  The location(s) provided must contain the source code (C libs) and sqlite3.exe (if they are in separate folders, provide multiple paths in the yaml).