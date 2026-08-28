# Revision history for agent-dsl

## 0.3.0.0 -- 2026-08-28

### Added

- Key-based credentials for servers and API endpoints. A declaration may name one after its URL, as `bearer env("TOKEN")` for `Authorization: Bearer <value>`, or `header "X-API-Key" env("KEY")` for a header you name. Secrets are read from the environment and never written in the workflow file, so a `.agent` file can be committed as it stands, and failures name the variable rather than its value.
- Credentials are resolved before anything is contacted, so a variable that is not set fails immediately instead of surfacing as a 401 partway through a run.
- `examples/auth-demo.agent`, which runs against httpbin with any values you care to set.

### Fixed

- A non-2xx reply from an MCP server was reported as `no JSON-RPC response with id 1 in the reply`, hiding the status. A 401 from a missing or wrong credential now reads `HTTP 401: {"error":"invalid_token",...}`.

## 0.2.0.0 -- 2026-08-27

### Added

- Selector paths on bindings — `answer.content[0].text`, `answer.structuredContent.result` — so a step can name the part of an earlier result it wants. A path that is not there fails the step rather than passing `null` along.
- Branch conditions beyond truthiness: `==` and `!=` against a JSON value, with `null`, `false`, `0`, `""`, `[]` and `{}` all counting as false.
- Plain HTTP API steps. An `apis:` block declares endpoints by base URL, and `api:<alias>.<method>(<path-parts>)` calls them, with `with <body>` for `post`, `put` and `patch`. A step's arguments join onto the base URL, so a value from an earlier call can be spliced into a path. A non-2xx status fails the step.
- The MCP `initialize` handshake, which was defined but never sent: the client now adopts the server's session id and negotiated protocol version as headers, and confirms with `notifications/initialized`. GitMCP and Chakra UI refuse `tools/list` outright without this.
- Selector paths are type-checked before execution, against the MCP result envelope and — where a server publishes one — the tool's `outputSchema`. Where the shape is not knowable the path is left to the runtime rather than guessed at.
- A per-session request id counter, and responses matched to the id they answer.
- Escapes inside string literals: `\"`, `\\`, `\n` and `\t`.
- New type errors: `TooManyArgs`, `UnboundVar`, `BadPath`, `ApiNotFound`, `UnknownMethod`, `BodyNotAllowed`, and duplicate server or API aliases.
- A test suite of 65 tests over the parser, type-checker, SSE framing, JSON-RPC decoding and argument binding, with a runner needing no dependency the executable does not already build against. It makes no network calls.
- Examples: `advanced-demo.agent` (the whole syntax), `many-servers-demo.agent` (seven MCP servers), `api-demo.agent` (HTTP endpoints mixed with MCP).

### Changed

Two changes alter what an existing `.agent` file means:

- A binding now holds the tool's `result` rather than the whole JSON-RPC envelope. Feeding a binding to a later step used to send an object where the tool wanted a string, which made threading one step's output into the next unusable.
- A branch arm's steps must be indented past the `if`. The arms were previously unbounded, so the `else` arm ran to the end of the file and no step could follow a branch.

Also:

- Sessions for all declared servers are opened concurrently. Seven servers: 29.7s to 22.6s, now bounded by the slowest server rather than their sum; six, excluding one pathologically slow server: 7.6s to 2.8s.
- `Step` carries a `Call` — a tool call or an API call — so binding scope, branches and selector paths work the same for both.
- Examples now quote their string arguments, matching the documented meaning of a bare identifier as a reference to an earlier binding.

### Fixed

- SSE responses were parsed by scanning for the first `d` and returning the rest of the body, so any stream carrying more than one event failed to decode — including the DeepWiki steps in the README's own example workflow. Replaced with a line-based event splitter.
- A JSON-RPC error, or a `tools/call` result flagged `isError`, was stored in the context as though the step had succeeded and fed to the next step.
- Arguments past the last required parameter were silently discarded, so a call went out missing an argument the author thought they had passed.
- An unknown binding name was sent to the server as the literal text of the identifier instead of being reported.
- A server URL kept its trailing `\r` on a CRLF-terminated file, so every schema fetch failed.
- Text the parser could not read after the last valid step was ignored rather than reported, and a file beginning with a blank line failed to parse at all.
- A repeated server alias silently left the earlier server unreachable.
- Every request went out with a hardcoded `"id": 1`.

### Removed

- `src/Context.hs`, which was empty and referenced from nowhere.
- The unused GET-based `fetchToolSchema`.

## 0.1.0.0 -- 2026-08-27

- First version. Released on an unsuspecting world.
- Core AST: `Step`, `Branch`, `Node`, `Workflow`, with support for nested branches.
- Parser (Megaparsec): identifiers, string literals, keywords, symbols, multi-arg tool calls, branches, and full multi-step workflows.
- Schema fetching: real JSON-RPC over HTTP to MCP servers, including SSE response framing and `initialize`/`tools/list` handshakes.
- Type-checker: validates a workflow's tool calls against fetched MCP schemas before execution, catching unknown tools and missing arguments.
- Interpreter: executes a validated workflow against a live MCP server, threading each step's output into subsequent steps via a runtime `Context`.
- Proved end-to-end against a real MCP server (Context7).
