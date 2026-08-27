# agent-dsl

A small, statically-typed language for defining and running AI agent workflows against [MCP](https://modelcontextprotocol.io) servers.

`.agent` files describe a sequence of tool calls — across one or more MCP servers — as plain text. Before anything runs, every tool call is validated against the target server's **real, live schema**. A broken or hallucinated tool reference is caught immediately, with a clear error, instead of failing mid-workflow after real network calls have already fired.

```
servers:
 docs = https://mcp.context7.com/mcp
 wiki = https://mcp.deepwiki.com/mcp

workflow ReactResearch:
 step resolve = tool:docs.resolve-library-id("react hooks", "react") -> libId
 step topics  = tool:wiki.read_wiki_structure("facebook/react") -> structure
 step insight = tool:wiki.ask_question("facebook/react", "What are the main new features in React 19?") -> answer
```

```bash
agent-dsl examples/linkedin-demo.agent
```

## Why

Most agent/LLM tool-calling happens live and non-deterministically — a model decides what to call, with what arguments, in the moment. That's flexible, but it means a hallucinated tool name or malformed argument only surfaces _after_ a wasted (sometimes costly) real API call.

`agent-dsl` takes the opposite approach: a workflow is a fixed, written plan, checked against the actual target server's schema **before execution**, then run deterministically. No LLM is involved in running a workflow — it's closer to Terraform (declare, validate, apply) than to a live agent loop.

## How it works

```
.agent file (text)
     │  Parser (Megaparsec)
     ▼
   AST
     │  fetch real schemas from each declared MCP server (JSON-RPC)
     ▼
Type-checker  ──▶  reports every broken tool call, or passes
     │
     ▼
Interpreter  ──▶  executes real tools/call requests, threading
                   each step's output into the next
```

Every stage is real and independently testable:

- **Parser** — hand-written recursive-descent combinators (Megaparsec), no code generation
- **Schema fetching** — real JSON-RPC over HTTP to live MCP servers, handling the SSE response framing MCP servers use
- **Type-checker** — pure; validates tool names and argument counts against fetched schemas, returns every error found, not just the first
- **Interpreter** — executes validated workflows, resolving `Var` references to prior steps' outputs and firing real `tools/call` requests

## Installation

Requires the Haskell toolchain: [GHCup](https://www.haskell.org/ghcup/) (installs GHC + Cabal).

```bash
git clone https://github.com/Turtle-dev3/agent-dsl.git
cd agent-dsl
cabal build
cabal install exe:agent-dsl --overwrite-policy=always
```

This installs `agent-dsl` onto your `PATH`. Verify with `agent-dsl` — you should see a usage message.

_(Pre-built binaries for common platforms are planned, so this won't always require the Haskell toolchain to be installed.)_

## Tests

```bash
cabal test
```

The suite covers the parser, type-checker, SSE framing and JSON-RPC decoding, and argument binding. It makes no network calls, so it runs offline.

## Usage

```bash
agent-dsl path/to/workflow.agent
```

The two example files in `examples/` are a good starting point:

- `examples/context7-demo.agent` — the smallest thing that runs: one server, one step
- `examples/linkedin-demo.agent` — a real, working multi-server workflow (Context7 + DeepWiki)
- `examples/advanced-demo.agent` — exercises the whole syntax: two servers, four tools, a nested branch, an escaped quote inside a string, and a step that picks up after the branch closes
- `examples/broken-demo.agent` — deliberately references a tool that doesn't exist, to demonstrate the type-checker catching it before execution

## Syntax

**Servers** — declare every MCP server a workflow talks to, with a local alias:

```
servers:
 <alias> = <url>
```

**Steps** — call a tool on a declared server, bind its result to a name:

```
step <name> = tool:<alias>.<tool-name>(<args>) -> <binding>
```

Arguments are either quoted string literals (`"like this"`) or references to an earlier step's binding. A binding must be defined by an earlier step; an unknown name is a type error, not a literal. Inside a string, `\"`, `\\`, `\n` and `\t` are escapes; a string may not span lines.

A binding holds the tool's result, so reach into it with `.field` and `[index]` to get at the part you want:

```
step ask = tool:wiki.ask_question("facebook/react", "Name one hook added in React 19.") -> answer
step follow = tool:wiki.ask_question("facebook/react", answer.content[0].text) -> detail
```

Selectors bind tightly — no spaces around them. A path that isn't there is a runtime error rather than a silent null.

**Branches** — conditionally run one of two step lists:

```
if <condition>:
 <steps>
else:
 <steps>
```

A condition is either a truthiness test on a value, or a comparison:

```
if answer.isError:                      # null, false, 0, "", [] and {} are false
if answer.content[0].type == "text":
if answer.content[0].type != "text":
```

Comparisons match JSON values, so a string literal only equals a JSON string. A selector that isn't there reads as null in a condition (rather than failing, as it would in an argument), so asking about a field a tool only sometimes returns is safe.

Each arm's steps must be indented past the `if`. The branch ends at the first line back at the `if`'s own indentation, so a workflow can carry on after it.

## Editor support

A companion VS Code extension ([`agent-dsl-vscode`](https://github.com/Turtle-dev3/agent-dsl-vscode)) provides:

- Syntax highlighting for `.agent` files
- A custom file icon
- A "run" button in the editor toolbar (shells out to the installed `agent-dsl` binary)

## Known limitations

This is a working proof of concept, not a production tool yet. Known gaps, honestly:

- **Positional, not named, argument matching** — arguments are matched to a schema's required parameters by position, not by name. The `.agent` file's argument order must match the schema's declared order.
- **Selector paths are checked at runtime, not by the type-checker** — a tool's output shape isn't in its schema, so `answer.content[0].text` can only fail once the step has run.
- **Branch conditions cover equality only** — `==` and `!=` against a JSON value, plus a truthiness test. No ordering (`greater than`), no boolean operators (`and`, `or`, `not`).
- **Runtime errors abort the workflow** — the interpreter uses `error` to stop on a failing step instead of the `Either`-based reporting the type-checker uses.
- **No authentication support yet** — no API keys, bearer tokens, or OAuth for MCP servers that require them.
- **MCP-only** — no support yet for calling plain REST/API endpoints outside the MCP protocol.

## Roadmap

- API-key auth for servers (`auth: env("...")`)
- A keyword letting an AI agent supply its own content into an otherwise fixed, validated workflow
- Generic (non-MCP) API call steps
- OAuth support for MCP servers that require it
- `agent-dsl` itself exposed as an MCP server (`validate_workflow` / `run_workflow` tools), so any MCP-aware AI agent can safely execute a structurally-validated plan without ever calling a broken tool

## License

MIT
