# QTreeInpla

QuadTree linear algebra implementation in Inpla.

# How to run golden tests

Ensure dotnet is installed.

1. Clone Inpla repository
2. Patch Inpla code to make MAX_PORTS >= 10, MAX_SYMBOLS >= 1024
3. Compile to obtain Inpla executable
4. Make sure you are in the project directory(Inpla's `use` directives are relative to current working directory)
5. `dotnet fsi test.fsx -- $PATH_TO_INPLA_EXECUTABLE` or simply `./test.fsx $PATH_TO_INPLA_EXECUTABLE`
