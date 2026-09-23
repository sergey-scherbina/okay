## ts-docs-guide - okay with TypeScript, reorganised by scenario

`docs/typescript.md` had grown one section per landed stage (T1–T11), in
the order the lanes landed. It is now a guide in the order of the three
questions a reader arrives with:
1. types written once;
2. a Scala backend with a TypeScript frontend;
3. both in the browser;
4. both on the backend.

A "Where to start" table maps what you have to what okay generates and to
the section.

- Every example and anchor is kept; the snippet check is unchanged and
  green.
- The page's introduction now presents the three scenarios, and the
  Node worker's introduction opens part 4.
- A stale limit ("okay-js prints JavaScript, not TypeScript… yet",
  untrue since ts-js-typed) is replaced by the limits that hold now.
- README.md and docs/README.md describe the page as it is.
