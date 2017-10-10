# regex-generator

Given a list of strings to match, generates some regular expressions that match them all.

It is also possible to provide a list of strings to not match, though this tends to be slow.

## Code

The code is written to run on SICStus Prolog.

To see some of what this program can do, read the tests or run them as follows:

```
prolog -l regex_test.pl 
run_tests(regex).
```

User-facing functions are in ```regex.pl```. Regular expression rules are in ```rules.pl```. 
Other files define the character sets and other utilities.
