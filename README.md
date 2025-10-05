# ocaml tennis

WIP. Eventually I'd like this to be a turn-based, probabilistic tennis shot selection game. For now, it just draws a singles tennis court with two players on it who move around each time you press a key.

This is also a for-fun project that is acting as an excuse to write some ocaml for the first time in a while.

## Building and running locally

(used Gemini Flash to generate this readme section, but I've tested it and it seems to work)

This project uses `opam` as its package manager and `dune` as its build system.

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/maxpblum/ocaml_tennis.git
    cd ocaml_tennis
    ```

2.  **Install dependencies:**
    * Ensure you have `opam` installed.
    * Create a local switch and install all project dependencies.
    ```bash
    opam switch create . --deps-only
    ```
    * Activate the local environment. You will need to run this command every time you open a new terminal in this directory.
    ```bash
    eval $(opam env)
    ```

3.  **Build the program:**
    ```bash
    dune build
    ```

4.  **Run the program:**
    ```bash
    dune exec ocaml_tennis
    ```
