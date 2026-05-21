# purescript-colog

A small, composable logging library for PureScript. Loggers are values you build
up and combine, so you can assemble exactly the logging you want from simple
pieces.

## What it can do

- Send logs to several destinations at once (console, a file, …).
- Transform, filter, and route messages on their way out.
- Severity levels, with coloured or plain output.
- Log from anywhere without threading a logger through your code.
- Enrich messages with structured fields like timestamps.
- Time how long an operation takes — safely, even when it throws.

## Inspiration

A port of the Haskell [`co-log`](https://github.com/co-log/co-log) and
[`co-log-core`](https://github.com/co-log/co-log-core) libraries, split the same
way into `colog-core` and `colog`.

## Example

[`examples/`](examples/) puts it all together in one script:

```bash
spago run -p examples
```
