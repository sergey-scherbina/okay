## docs-python-r - the Python and R guide, for readers arriving from outside

docs/python-and-r.md tells the Python/R story end to end for a reader
who has never seen okay:

- a call and a call back, in a small shop example, in both languages;
- what the NAME in `okay.call("price_of", x)` is (a label chosen on the
  Scala side, offered to one call, checked at the boundary) and why it
  is a name rather than a function;
- a Mermaid picture of the process boundary and a sequence diagram of
  the dialogue, both drawn by GitHub;
- the rest of the toolkit, what stays safe, the limits, and literature.

The page is pinned in TestDocSnippets, so every Scala, Python and R line
in it is a verbatim line of a test. A planted line fails the check. The
shop is new as TestPyShop and TestRShop (live), including the refusal of
a mistyped name that lists what was offered. Linked from README "Start
here", docs/README.md and docs/jvm-languages.md. The README row that
still called Python and R call-only handlers now points to the new page.
