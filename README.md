# agent-dsl

A small, statically-typed language for defining and running AI agent workflows against [MCP](https://modelcontextprotocol.io) servers.

An agent-dsl workflow is a **capability**: a fixed plan an AI agent can invoke as a single tool, without choosing what calls it makes. `agent-dsl --serve <dir>` offers a directory of workflows over MCP, one tool per workflow, each type-checked against its servers before it runs.

`.agent` files describe a sequence of calls — across one or more MCP servers, plain HTTP APIs, or both — as plain text. Before anything runs, every tool call is validated against the target server's **real, live schema**. A broken or hallucinated tool reference is caught immediately, with a clear error, instead of failing mid-workflow after real network calls have already fired.

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
- **Type-checker** — pure; validates tool names, argument counts, binding references and selector paths against fetched schemas, returns every error found, not just the first
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
- `examples/advanced-demo.agent` — exercises the whole syntax: two servers, four tools, one step's answer fed into the next through a selector path, a truthiness branch and a nested comparison branch, and a step that picks up after the branch closes
- `examples/many-servers-demo.agent` — seven MCP servers in one workflow: DeepWiki, Context7, GitMCP, Microsoft Learn, Cloudflare Docs, Svelte and Chakra UI
- `examples/api-demo.agent` — mixes plain HTTP endpoints (GitHub, Hacker News, npm) with an MCP server, and splices a Hacker News story id into a URL path
- `examples/auth-demo.agent` — bearer and named-header credentials against httpbin; run it with `DEMO_TOKEN` and `DEMO_KEY` set to anything
- `examples/broken-demo.agent` — deliberately references a tool that doesn't exist, to demonstrate the type-checker catching it before execution

## Syntax

**Servers** — declare every MCP server a workflow talks to, with a local alias:

```
servers:
 <alias> = <url>
```

**APIs** — declare plain HTTP endpoints the same way, by base URL:

```
apis:
 <alias> = <base-url>
```

Both blocks are optional and may appear in either order, so a workflow can be all MCP, all HTTP, or a mix of the two.

**Credentials** — a declaration that needs a key names one after its URL, in either of two forms:

```
servers:
 linear = https://mcp.linear.app/mcp bearer env("LINEAR_KEY")

apis:
 github = https://api.github.com bearer env("GITHUB_TOKEN")
 weather = https://api.example.com header "X-API-Key" env("WEATHER_KEY")
```

`bearer` sends `Authorization: Bearer <value>`; `header` sends a header you name. A secret is always read from the environment and never written in the file, so a workflow can be committed as it stands. Every credential is resolved before anything is contacted, so a variable you forgot to set fails immediately:

```
API github: environment variable GITHUB_TOKEN is not set
```

Failures name the variable, never its value.

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

Paths are type-checked before the workflow runs, against the MCP result envelope (`content`, `structuredContent`, `isError`, `_meta`) and, where the server publishes an `outputSchema` for the tool, against that too:

```
Workflow failed type-checking:
BadPath "typo" "answer.contnt[0].text" "has no field \"contnt\"; it has \"_meta\", \"content\", \"isError\", \"structuredContent\""
BadPath "deep" "answer.structuredContent.nope" "has no field \"nope\"; it has \"result\""
```

Where a server publishes no output schema, the shape of `structuredContent` is unknown and paths into it fall back to being caught at runtime. Paths in a *branch condition* are not checked, since a condition reads a missing path as null by design.

**API steps** — call an endpoint. The arguments join onto the base URL to form the path, so a value from an earlier step can be spliced into it:

```
step <name> = api:<alias>.<method>(<path-parts>) -> <binding>
step <name> = api:<alias>.<method>(<path-parts>) with <body> -> <binding>
```

```
step top = api:hn.get("/v0/topstories.json") -> stories
step story = api:hn.get("/v0/item/", stories[0], ".json") -> story
step created = api:github.post("/repos/o/r/issues") with "{\"title\":\"hi\"}" -> issue
```

Methods are `get`, `post`, `put`, `patch`, `delete` and `head`; only `post`, `put` and `patch` accept a `with` body. A JSON response is bound as parsed JSON, anything else as a string, and a non-2xx status fails the step the way an MCP error does.

The type-checker still catches an undeclared alias, an unknown method, a body on a method that takes none, and a reference to a binding that does not exist — but an HTTP endpoint publishes no schema, so argument counts and selector paths into an API result cannot be checked ahead of time the way MCP tool calls are.

**Output** — name what the workflow reports when it finishes:

```
output:
 <name> = <input>
```

```
output:
 topStory = story.title
 latestRelease = latest.tag_name
```

The result is a JSON object of exactly those values, on stdout. Everything else — progress, errors — goes to stderr, so a workflow pipes straight into whatever asked for it:

```bash
agent-dsl examples/api-demo.agent | jq .latestRelease
```

Output paths are checked before the workflow runs, like any other reference. A workflow that names no output reports every binding it made, which is useful while writing one and rarely what you want afterwards.

**Input** — values the caller supplies, in scope from the first step:

```
workflow RepoHealth:
 input:
  owner "the GitHub organisation or user"
  repo "the repository name"

 step details = api:github.get("/repos/", owner, "/", repo) -> info
```

```bash
agent-dsl repo-health.agent owner=facebook repo=react
```

A missing input is refused before anything runs. When the workflow is served over MCP these become the tool's arguments, so an agent sees a capability with a signature.

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

## Serving workflows to an agent

```bash
agent-dsl --serve examples/capabilities
```

This speaks MCP over stdio and offers one tool per `.agent` file in the directory. A workflow's `input:` block becomes the tool's arguments and its `output:` block becomes the result, so an agent sees:

```
tool: repo-health | args: ['owner', 'repo']
      Runs the fixed, pre-validated workflow RepoHealth (2 step(s)), reporting:
      stars openIssues latestRelease latestReleaseDate. Every call it makes is
      checked against its servers' schemas before any of them run.
```

The point is what the agent *cannot* do. It picks which capability to invoke; it never picks the calls that capability makes. Those were written by a person, committed to a repository, and checked against live schemas before any of them ran. A failing workflow comes back as a tool error rather than taking the server down with it.

To use it from a client, point that client at `agent-dsl --serve <your-workflow-directory>` as an MCP server.

## Editor support

`agent-dsl --lsp` is a language server for `.agent` files. It offers:

- **Diagnostics** — everything the type-checker finds, on the line it belongs to, as you type: an unknown tool, the wrong number of arguments, a binding that was never defined, a path a result cannot have.
- **Completion of a path into an earlier result.** After `answer.` it offers the fields that result actually has. For an MCP tool this comes from the server's schema, so it works before the workflow has ever run. For an HTTP endpoint, which publishes no schema, it comes from the shape recorded on the last run — what the API returned, not what it documents.
- **Completion of aliases, tool names and HTTP methods** after `tool:`, `tool:alias.`, `api:` and `api:alias.`.

Running a workflow writes a `.agent-dsl-shapes.json` beside it, recording the shape of each binding. It feeds completion only, never the type-checker, so a stale entry can never turn into a false error. Delete it freely; the next run writes it again.

Each run replaces what that workflow recorded, so a binding you rename stops being suggested — and a branch only ever contributes the arm the last run actually took.

Schemas are fetched when a file is opened and when it is saved, never per keystroke.

## Editor support

A companion VS Code extension ([`agent-dsl-vscode`](https://github.com/Turtle-dev3/agent-dsl-vscode)) provides:

- Syntax highlighting for `.agent` files
- A custom file icon
- A "run" button in the editor toolbar (shells out to the installed `agent-dsl` binary)
- Diagnostics and completion, by running `agent-dsl --lsp`

## Known limitations

This is a working proof of concept, not a production tool yet. Known gaps, honestly:

- **Positional, not named, argument matching** — arguments are matched to a schema's required parameters by position, not by name. The `.agent` file's argument order must match the schema's declared order.
- **Paths into `structuredContent` are only checked for servers that publish an `outputSchema`** — for the rest, the shape isn't knowable ahead of time and the path is caught at runtime instead.
- **Branch conditions cover equality only** — `==` and `!=` against a JSON value, plus a truthiness test. No ordering (`greater than`), no boolean operators (`and`, `or`, `not`).
- **No retries, loops or error branches** — a failing step stops the workflow and reports why. Real automation wants to retry a flaky call or iterate over a list, and neither is expressible yet.
- **Key-based auth only** — a bearer token or a named header, read from the environment. No OAuth, so servers that require an authorization-code flow are still out of reach.

## Roadmap

- A keyword letting an AI agent supply its own content into an otherwise fixed, validated workflow
- OAuth support for MCP servers that require it
- Retries and error branches, so a flaky step need not end a run
- Iteration, so a workflow can act on each item of a list

## License

MIT
