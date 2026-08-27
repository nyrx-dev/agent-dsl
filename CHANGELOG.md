# Revision history for agent-dsl

## 0.1.0.0 -- 2026-08-27

- First version. Released on an unsuspecting world.
- Core AST: `Step`, `Branch`, `Node`, `Workflow`, with support for nested branches.
- Parser (Megaparsec): identifiers, string literals, keywords, symbols, multi-arg tool calls, branches, and full multi-step workflows.
- Schema fetching: real JSON-RPC over HTTP to MCP servers, including SSE response framing and `initialize`/`tools/list` handshakes.
- Type-checker: validates a workflow's tool calls against fetched MCP schemas before execution, catching unknown tools and missing arguments.
- Interpreter: executes a validated workflow against a live MCP server, threading each step's output into subsequent steps via a runtime `Context`.
- Proved end-to-end against a real MCP server (Context7).
