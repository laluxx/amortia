<div align="center">
  <img src="logo-white.svg" alt="Amortia Logo" width="200"/>
  
  # Amortia
  
  ### A Pure Functional Language for the BEAM VM
  
  [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
  [![Made with Haskell](https://img.shields.io/badge/Made%20with-Haskell-5e5086?logo=haskell)](https://www.haskell.org/)
  
  *Elegance meets the power of the BEAM*
</div>

---

## What is Amortia?

Amortia is a pure functional programming language designed to compile to the BEAM VM. It combines Haskell and Lisp syntax with the battle-tested concurrency and fault-tolerance of Erlang's virtual machine.

**Current Status:** Parser & AST Visualizer
The compiler pipeline is under active development. Right now, you can parse Amortia code, generate AST in JSON format, and explore it with an interactive hot-reloading visualizer.

### Compilation Pipeline

<table>
<tr>
<td align="center" width="20%">
  
**Amortia**<br/>
`.amor`
  
</td>
<td align="center" width="5%">
  
→<br/>
✅
  
</td>
<td align="center" width="20%">
  
**AST**<br/>
`JSON`
  
</td>
<td align="center" width="5%">
  
→<br/>
🚧
  
</td>
<td align="center" width="20%">
  
**Erlang**<br/>
`.erl`
  
</td>
<td align="center" width="5%">
  
→
  
</td>
<td align="center" width="20%">
  
**Core**<br/>
`.core`
  
</td>
<td align="center" width="5%">
  
→
  
</td>
<td align="center" width="20%">
  
**BEAM**<br/>
`.beam`
  
</td>
</tr>
</table>

---

## Quick Start

> [!NOTE]
> Amortia requires GHC (Glasgow Haskell Compiler) >= 9.0 and Make

### Installation

```bash
git clone https://github.com/yourusername/amortia.git
cd amortia
make release
sudo make install  # optional
```

### Example

Create a file `example.amor`:

```haskell
defn last :: [a] -> a {
  "Returns the last element of a list.
   Raises badarg error if the list is empty."
    [] -> error badarg
    [x] -> x
    [_|xs] -> last xs
}
```

### Usage

```bash
# Watch mode with hot reloading
amortia --watch example.amor

# Visualize once
amortia --visualize example.amor

# Export to JSON
amortia --json example.amor output.json
```

---

## 📖 Language Features

### Pattern Matching

```haskell
defn length :: [a] -> Int {
  "Calculate the length of a list"
    [] -> 0
    [_|xs] -> 1 + length xs
}
```

| Pattern   | Description      |
|-----------|------------------|
| `x`       | Variable binding |
| `_`       | Wildcard         |
| `[]`      | Empty list       |
| `[x\|xs]` | Head and tail    |
| `42`      | Literal match    |

### Type System

Hindley-Milner style with parametric polymorphism:

```haskell
defn map :: (a -> b) -> [a] -> [b] {
  "Apply a function to each element of a list"
    f [] -> []
    f [x|xs] -> [f x | map f xs]
}
```

---

## AST Visualizer

> [!TIP]
> Use watch mode for the best development experience with instant feedback!

The interactive visualizer provides:
- Side-by-side source code and AST tree view
- Syntax highlighting
- Hot reloading - changes reflect instantly
- Collapsible tree nodes
- JSON export toggle

```bash
amortia --watch mycode.amor
```

---

## Development

### Building

```bash
make          # Development build
make release  # Optimized build
make clean    # Clean artifacts
```

### Project Structure

```
amortia/
├── Parser.hs          # Megaparsec-based parser
├── Main.hs            # CLI tool & visualizer
├── visualizer.html    # Interactive AST viewer
└── Makefile           # Build configuration
```

---

## 🗺️ Roadmap

<details open>
<summary><b>Phase 1: Parser & Tooling</b> ✓</summary>

- ✅ Complete parser
- ✅ AST generation and JSON export
- ✅ Interactive visualizer with hot-reloading

</details>

<details>
<summary><b>Phase 2: Compiler</b> (In Progress)</summary>

- ⏳ Type checker and inference
- ⏳ Erlang code generation
- ⏳ Standard library

</details>

<details>
<summary><b>Phase 3: BEAM Integration</b> (Planned)</summary>

- 📋 Core Erlang emission
- 📋 Direct BEAM bytecode generation
- 📋 Interop with Erlang/Elixir

</details>

---

## License

MIT License - see LICENSE file for details

---

<div align="center">
  
  ### Built with ❤️ and Haskell
  
</div>
