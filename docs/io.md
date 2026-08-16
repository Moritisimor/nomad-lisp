# Input/Output

## print
Prints one or more expressions to stdout.

### Signature
```lisp
(print <expressions...>)
```

Returns `unit`

## println
Exactly the same as print except with a newline at the end.

## readln
Reads a line from stdin (until enter is pressed)

### Signature
```lisp
(readln <prompt: string>)
```

Returns `string`

## exec
Executes a program

The program is launched via the system shell. On UNIX-like OSes that's `sh`, on windows it's `cmd.exe`

The exit code is returned

### Signature
```lisp
(exec <program: string>)
```

Returns `number`

## read_file
Reads a file.

### Signature
```lisp
(read_file <path: string>)
```

This function may throw if there is no file at that path, the path is a directory or the user doesn't have the necessary permissions.

Returns `string`

## write_file
Writes a string to a file, creating it if it doesn't already exist

### Signature
```lisp
(write_file <path: string> <content: string>)
```

This function may throw if the file is at a non-existant path or the user doesn't have the necessary permissions

Returns `string`

## remove_file
Removes a file

### Signature
```lisp
(remove_file <path: string>)
```

This function may throw if the file is at a non-existant path or the user doesn't have the necessary permissions

Returns `unit`

## read_dir
Reads a directory, returning the Filesystem-node names as a list of strings

```lisp
(read_dir <path: string>)
```

This function may throw if the directory doesn't exist, is a file or the user doesn't have the necessary permissions

Returns `string list`

## mkdir
Creates a directory

```lisp
(mkdir <path: string>)
```

This function may throw if directory is at a non-existant path or the user doesn't have the necessary permissions

Returns `unit`

## remove_dir
Removes a directory

```lisp
(remove_dir <path: string>)
```

This function may throw if directory is at a non-existant path or the user doesn't have the necessary permissions

Returns `unit`

## chdir
Changes the current working directory of the current process

```lisp
(chdir <path: string>)
```

This function may throw if path doesn't exist

Returns `unit`

## cwd
Returns the current working directory

```lisp
(cwd)
```

Returns `string`

## get_env
Reads an environment variable

```lisp
(get_env <variable: string>)
```

This function may throw if the variable doesn't exist

Returns `string`

## get_env_unit
Reads an environment variable, however, instead of throwing if the variable doesn't exist, unit is returned

```lisp
(get_env_unit <variable: string>)
```

Returns `string` | `unit`

## exit
Exits the current process with a code

```lisp
(exit <code: number>)
```

Returns nothing, as the process is exited

## bye
Shorthand for `(exit 0)`

```lisp
(bye)
```

Returns nothing, as the process is exited
