import VersoBlog
import Blog

open Verso Genre Blog Site Syntax
open Output Html Template Theme

private def pageClass (path : List String) : String :=
  match path with
  | [] => "page-home"
  | xs => "page-" ++ String.intercalate "-" xs

private def proofLightStyle := r#"
:root {
  --page-width: min(118ch, calc(100vw - 3.5rem));
  --page-padding: clamp(1.25rem, 2vw, 2rem);
  --bg: #f5f1e8;
  --panel: #fbf8f2;
  --text: #2c2620;
  --muted: #6d6358;
  --border: #d7cec0;
  --accent: #3D755D;
  --accent-soft: #e2efe7;
  --code-bg: #ece7dd;
  --verso-text-font-family: "Iowan Old Style", "Palatino Linotype", "Book Antiqua", Georgia, serif;
  --verso-structure-font-family: "Iowan Old Style", "Palatino Linotype", "Book Antiqua", Georgia, serif;
  --verso-code-font-family: "SFMono-Regular", "SF Mono", Menlo, Consolas, monospace;
  --verso-text-color: var(--text);
  --verso-code-color: var(--text);
  --verso-structure-color: var(--text);
}

* {
  box-sizing: border-box;
}

html {
  background: var(--bg);
}

body {
  margin: 0;
  background: var(--bg);
  color: var(--text);
  line-height: 1.72;
  padding: var(--page-padding);
}

.site-shell {
  width: var(--page-width);
  margin: 0 auto;
}

.site-header {
  margin-bottom: 2.4rem;
  padding-bottom: 0.9rem;
  border-bottom: 1px solid var(--border);
}

.site-header-inner {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 1rem 1.5rem;
  flex-wrap: wrap;
}

.site-title {
  color: var(--text);
  text-decoration: none;
  font-size: 1.05rem;
  font-weight: 600;
  letter-spacing: 0.01em;
}

.site-title:hover {
  color: var(--accent);
}

nav.top {
  margin: 0;
  padding: 0;
  border: 0;
}

nav.top ol {
  list-style: none;
  display: flex;
  flex-wrap: wrap;
  gap: 0.9rem 1.2rem;
  margin: 0;
  padding: 0;
}

nav.top li {
  margin: 0;
}

nav.top a {
  color: var(--muted);
  text-decoration: none;
  font-size: 0.97rem;
}

nav.top a:hover {
  color: var(--accent);
}

main article {
  background: transparent;
}

h1, h2, h3, h4, h5, h6 {
  color: var(--text);
  line-height: 1.18;
  margin: 2rem 0 0.65rem;
}

h1 {
  font-size: 2.15rem;
  margin-top: 0;
}

h2 {
  font-size: 1.35rem;
}

h3 {
  font-size: 1.08rem;
}

p, ul, ol, pre, table {
  margin: 0 0 1rem;
}

ul, ol {
  padding-left: 1.35rem;
}

li {
  margin: 0.3rem 0;
}

a {
  color: var(--accent);
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

code {
  font-family: var(--verso-code-font-family);
  background: var(--code-bg);
  border-radius: 0.35rem;
  padding: 0.12rem 0.35rem;
  font-size: 0.92em;
}

pre {
  background: var(--code-bg);
  border: 1px solid var(--border);
  border-radius: 0.7rem;
  padding: 0.95rem 1rem;
  overflow-x: auto;
}

pre code {
  background: transparent;
  padding: 0;
}

blockquote {
  margin: 1.25rem 0;
  padding: 0.15rem 0 0.15rem 1rem;
  border-left: 3px solid var(--border);
  color: var(--muted);
}

table {
  width: 100%;
  border-collapse: collapse;
}

th, td {
  text-align: left;
  padding: 0.55rem 0.5rem;
  border-bottom: 1px solid var(--border);
}

th {
  color: var(--muted);
  font-weight: 600;
}

.site-footer {
  margin-top: 3rem;
  padding-top: 1rem;
  border-top: 1px solid var(--border);
  color: var(--muted);
  font-size: 0.92rem;
}

.site-footer p {
  margin: 0;
}

body.page-home main article section:last-of-type ul {
  list-style: none;
  display: flex;
  gap: 0.75rem 1.1rem;
  flex-wrap: wrap;
  padding: 0;
}

body.page-home main article section:last-of-type li,
body.page-home main article section:last-of-type p {
  margin: 0;
}

body.page-home main article section:last-of-type a {
  display: inline-block;
  padding: 0.38rem 0.72rem;
  background: var(--accent-soft);
  border: 1px solid #bdd4c8;
  border-radius: 999px;
  color: var(--accent);
}

body.page-home main article section:last-of-type a:hover {
  text-decoration: none;
  background: #d4e6dc;
}

body.page-teaching main article {
  font-size: 0.94rem;
}

body.page-teaching h1 {
  font-size: 1.8rem;
}

body.page-teaching h2 {
  font-size: 1.08rem;
}

@media (max-width: 700px) {
  :root {
    --page-width: calc(100vw - 2rem);
  }

  body {
    padding: 1rem;
  }

  .site-header {
    margin-bottom: 1.8rem;
  }

  h1 {
    font-size: 1.85rem;
  }
}
"#

def websiteTheme : Theme := { Theme.default with
  primaryTemplate := do
    let path := (← currentPath).toList
    let siteRoot := String.join (path.map fun _ => "../") ++ "./"
    let postList :=
      match (← param? "posts") with
      | none => Html.empty
      | some html => {{ <h2> "Posts" </h2> }} ++ html
    let catList :=
      match (← param? (α := Post.Categories) "categories") with
      | none => Html.empty
      | some ⟨cats⟩ => {{
          <section class="categories">
            <h2> "Categories" </h2>
            <ul>
            {{ cats.map fun (target, cat) =>
              {{ <li><a href={{target}}>{{Post.Category.name cat}}</a></li> }}
            }}
            </ul>
          </section>
        }}
    return {{
      <html>
        <head>
          <meta charset="utf-8"/>
          <meta name="viewport" content="width=device-width, initial-scale=1"/>
          <meta name="color-scheme" content="light"/>
          <link rel="icon" href="data:," />
          <style>":root { --justify-important: left; }"</style>
          <style>{{proofLightStyle}}</style>
          <title>{{← param (α := String) "title"}} " | Jyun-Ao Lin"</title>
          {{← builtinHeader}}
        </head>
        <body class={{pageClass path}}>
          <div class="site-shell">
            <header class="site-header">
              <div class="site-header-inner">
                <a class="site-title" href={{siteRoot}}>"Jyun-Ao Lin"</a>
                {{← topNav}}
              </div>
            </header>
            <main>
              {{← param "content"}}
              {{postList}}
              {{catList}}
            </main>
            <footer class="site-footer">
              <p>
                "Powered by "
                <a href="https://github.com/leanprover/verso">"Verso"</a>
                " and "
                <a href="https://lean-lang.org/">"Lean 4"</a>
                "."
              </p>
            </footer>
          </div>
        </body>
      </html>
    }}
}

def website : Site := site Blog.FrontPage /
  "about" Blog.About
  "academic" Blog.Academic
  "teaching" Blog.Teaching

def main := blogMain websiteTheme website
