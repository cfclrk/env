# envars

An Emacs package that provides an API to set and unset environment variables
from files, strings, and pairs.

This package also provides two interactive functions:

1. `envars-set-file`: Set all the environment variables defined in an env file
2. `envars-unset-file`: Unset all the environment variables defined in an env
    file

When used interactively, those functions prompt for a file. By default, the
prompt begins at `envars-dir`.

## Installation

Example using straight:

```emacs-lisp
(straight-use-package
 '(envars :type git
          :host github
          :repo "cfclrk/envars"))
```

## Usage

Create a file in `envars-dir` (which is by default `~/.env/`). For example,
create this file in `~/.env/foo`:

```sh
FOO=~/foo
BAR=$FOO/bar
ОФИС=ДОМ
BAZ=nosubst:FOO$BAR
```

Now, you can run:

- `M-x envars-set-file`, which will prompt you for a file. All the environment
  variables defined in the file will be **set**.
- `M-x envars-unset-file`, which will prompt you for a file. All the
  environment variables defined in the file will be **unset**.

### Usage from Elisp

To set env variables defined in `~/.env/foo`:

```emacs-lisp
(envars-set-file (expand-file-name "~/.env/foo"))
```

Or, if you have a string instead of a file:

```emacs-lisp
(envars-set-str "FOO=foo\nBAR=bar")
```

## Usage from org-mode

The example below shows a convenient way to declare and set environment
variables in an `org` document:

```org
#+NAME: env
| Var  | Value           |
|------+-----------------|
| FOO  | ~/foo           |
| BAR  | $FOO/bar        |
| ОФИС | ДОМ             |
| BAZ  | nosubst:FOO$BAR |

#+begin_src emacs-lisp :var env=env
  (envars-set-pairs env)
#+end_src
```

# File Format

Each line in the file must be in a `KEY=VALUE` format, with one entry per line.
This package invokes an `sh` shell to interpret the file, so shell-isms
technically should work. However, for compatability with other tools, it's best
to stick to the following minimal set of features:

- Use existing environment variables
- Define an environment variable and use it in successive lines
- A `~` is expanded if it is the first character in the value

# How it Works

The first thing this package does is convert input into a list of key/value
pairs (call it the "internal representation" or IR).

1. Parse input into an IR (a list of key/value pairs)
2. Run pre-eval hooks on the IR
3. Assemble into a shell script, evaluate, and parse result back into IR
4. Run post-eval hooks on the IR
5. Set (or return) the resulting list of key/value pairs as environment variables

The way in:

```mermaid
flowchart LR
    input -- parse --> IR
    IR -- pre-eval-hooks --> IR
    IR -- assemble --> shell-script
    shell-script -- evaluate --> output
```

The way out:

```mermaid
flowchart LR
  output -- parse --> IR
  IR -- post-eval-hooks --> IR
  IR -- export--> done
```

# Development

1. `make dep`: Install dependencies
2. `make test`: Run unit tests (you must run `make dep` first!)

# See Also

- [emacs-direnv](https://github.com/wbolster/emacs-direnv)
- [envrc](https://github.com/purcell/envrc)
- [parsenv](https://github.com/articuluxe/parsenv)
