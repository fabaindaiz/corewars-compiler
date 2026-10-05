#!/usr/bin/env python3
"""Build one HTML page from the user manual (docs/manual/*.md), for publishing.

    python3 tools/manual_page.py [OUT.html]      default: _build/manual-page/index.html

The Markdown is the source (d-7d2612-c06299); this page is made from it and never edited by hand.
It reads the small Markdown subset the manual uses: headings, paragraphs, lists, tables, fenced
code, inline code, bold and links. A `red` block followed by its `redcode` block is shown side by
side. A link to another chapter becomes an anchor in the page; a link to a repository file becomes
the file's path, since the page leaves the repository.
"""
from __future__ import annotations

import html
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MANUAL = ROOT / "docs" / "manual"


def slug(text: str) -> str:
    return re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")


def chapter_id(name: str) -> str:
    return "ch-" + Path(name).stem.lower()


def inline(text: str, chapter: str) -> str:
    """Inline code, bold and links; everything else escaped."""
    out, i = [], 0
    for m in re.finditer(r"`([^`]+)`|\*\*([^*]+)\*\*|\[([^\]]+)\]\(([^)]+)\)", text):
        out.append(html.escape(text[i:m.start()]))
        if m.group(1) is not None:
            out.append(f"<code>{html.escape(m.group(1))}</code>")
        elif m.group(2) is not None:
            out.append(f"<strong>{inline(m.group(2), chapter)}</strong>")
        else:
            label, target = m.group(3), m.group(4)
            if target.startswith("http"):
                out.append(f'<a href="{html.escape(target)}">{inline(label, chapter)}</a>')
            else:
                path, _, frag = target.partition("#")
                if path.endswith(".md") and (MANUAL / path).resolve().parent == MANUAL and (MANUAL / path).exists():
                    anchor = frag if frag else chapter_id(path)
                    out.append(f'<a href="#{html.escape(anchor)}">{inline(label, chapter)}</a>')
                elif not path:
                    out.append(f'<a href="#{html.escape(frag)}">{inline(label, chapter)}</a>')
                else:
                    # a file of the repository: its path, as text
                    try:
                        shown = str((MANUAL / path).resolve().relative_to(ROOT))
                    except ValueError:
                        shown = path
                    out.append(f'{inline(label, chapter)} <span class="path">({html.escape(shown)})</span>'
                               if label.strip("`") != shown else f"<code>{html.escape(shown)}</code>")
        i = m.end()
    out.append(html.escape(text[i:]))
    return "".join(out)


RED_WORDS = {"seq", "let", "label", "com", "store", "repeat", "if", "while", "do-while", "program",
             "define", "for", "include", "const", "hill", "name", "author", "strategy", "optimize",
             "expect", "start"}


def highlight_red(code: str) -> str:
    def token(m: re.Match) -> str:
        t = m.group(0)
        e = html.escape(t)
        if t.startswith('"'):
            return f'<span class="s">{e}</span>'
        if re.fullmatch(r"-?\d+", t):
            return f'<span class="n">{e}</span>'
        if t in RED_WORDS:
            return f'<span class="k">{e}</span>'
        if re.fullmatch(r"[A-Z][A-Za-z]*", t):
            return f'<span class="op">{e}</span>'
        return e
    return re.sub(r'"[^"]*"|[^\s()]+', token, code)


def highlight_redcode(code: str) -> str:
    lines = []
    for line in code.split("\n"):
        if line.startswith(";"):
            lines.append(f'<span class="c">{html.escape(line)}</span>')
        elif line and not line.startswith(" "):
            lines.append(f'<span class="l">{html.escape(line)}</span>')
        else:
            e = html.escape(line)
            e = re.sub(r"^(\s+)([A-Z]{3})(\.[A-Z]+)?",
                       lambda m: f'{m.group(1)}<span class="op">{m.group(2)}</span>'
                                 f'<span class="m">{m.group(3) or ""}</span>', e)
            lines.append(e)
    return "\n".join(lines)


def code_block(info: str, body: str) -> str:
    if info == "red":
        return f'<figure class="code red"><figcaption>RED</figcaption><pre>{highlight_red(body)}</pre></figure>'
    if info == "redcode":
        return (f'<figure class="code redcode"><figcaption>redcode, as compiled</figcaption>'
                f'<pre>{highlight_redcode(body)}</pre></figure>')
    label = {"sh": "shell", "text": "output"}.get(info, info)
    return f'<figure class="code plain"><figcaption>{html.escape(label)}</figcaption><pre>{html.escape(body)}</pre></figure>'


def render(md: str, chapter: str) -> tuple[str, str, list[tuple[str, str]]]:
    """The chapter's HTML, its title, and its sections (id, title) for the contents."""
    lines = md.split("\n")
    out: list[str] = []
    sections: list[tuple[str, str]] = []
    title = ""
    i = 0
    pending_red: str | None = None  # a red block waiting to see whether its redcode follows

    def flush_red():
        nonlocal pending_red
        if pending_red is not None:
            out.append(code_block("red", pending_red))
            pending_red = None

    while i < len(lines):
        line = lines[i]
        if line.startswith("```"):
            info = line[3:].strip()
            j = i + 1
            while lines[j].strip() != "```":
                j += 1
            body = "\n".join(lines[i + 1:j])
            i = j + 1
            if info == "red":
                flush_red()
                pending_red = body
            elif info == "redcode" and pending_red is not None:
                out.append(f'<div class="pair">{code_block("red", pending_red)}<div class="arrow" aria-hidden="true">→</div>'
                           f'{code_block("redcode", body)}</div>')
                pending_red = None
            else:
                flush_red()
                out.append(code_block(info, body))
            continue
        if line.strip() == "":
            i += 1
            continue
        flush_red()
        m = re.match(r"^(#{1,4}) (.*)$", line)
        if m:
            level, text = len(m.group(1)), m.group(2)
            if level == 1:
                title = text
                out.append(f'<h2 id="{chapter_id(chapter)}">{inline(text, chapter)}</h2>')
            else:
                sid = slug(text)
                if level == 2:
                    sections.append((sid, text))
                out.append(f'<h{level + 1} id="{sid}">{inline(text, chapter)}</h{level + 1}>')
            i += 1
            continue
        if line.startswith("|"):
            rows = []
            while i < len(lines) and lines[i].startswith("|"):
                rows.append([c.strip() for c in lines[i].strip().strip("|").split("|")])
                i += 1
            head, body_rows = rows[0], [r for r in rows[2:]]
            t = ['<div class="table"><table><thead><tr>']
            t += [f"<th>{inline(c, chapter)}</th>" for c in head]
            t.append("</tr></thead><tbody>")
            for r in body_rows:
                t.append("<tr>" + "".join(f"<td>{inline(c, chapter)}</td>" for c in r) + "</tr>")
            t.append("</tbody></table></div>")
            out.append("".join(t))
            continue
        if line.startswith("- "):
            items = []
            while i < len(lines) and (lines[i].startswith("- ") or lines[i].startswith("  ")) and lines[i].strip():
                if lines[i].startswith("- "):
                    items.append(lines[i][2:])
                else:
                    items[-1] += " " + lines[i].strip()
                i += 1
            out.append("<ul>" + "".join(f"<li>{inline(it, chapter)}</li>" for it in items) + "</ul>")
            continue
        para = []
        while i < len(lines) and lines[i].strip() and not re.match(r"^(#{1,4} |```|\||- )", lines[i]):
            para.append(lines[i].strip())
            i += 1
        out.append(f"<p>{inline(' '.join(para), chapter)}</p>")
    flush_red()
    return "\n".join(out), title, sections


STYLE = """
/* Layout: a fixed contents rail beside one reading column; collapses to a single column on phones. */
:root {
  --bg: #f7f8f6; --surface: #ffffff; --fg: #1d2421; --muted: #5c6863; --rule: #d9dfdb;
  --accent: #1f6f5c; --code-bg: #eef2ef; --red-bg: #f1f4ee; --rc-bg: #edf1f4;
  --k: #1f6f5c; --op: #2b4f86; --n: #9a4b16; --s: #7a3d8f; --c: #6f7b75; --l: #8a2f45; --m: #6f7b75;
  --display: "IBM Plex Sans Condensed", "Arial Narrow", system-ui, sans-serif;
  --body: "IBM Plex Sans", system-ui, -apple-system, "Segoe UI", sans-serif;
  --mono: "IBM Plex Mono", ui-monospace, "SF Mono", Menlo, Consolas, monospace;
}
@media (prefers-color-scheme: dark) { :root:not([data-theme="light"]) {
  --bg: #121614; --surface: #181d1b; --fg: #e3e8e5; --muted: #9aa6a0; --rule: #2a322e;
  --accent: #6cc4a8; --code-bg: #1b2120; --red-bg: #19201c; --rc-bg: #181e23;
  --k: #6cc4a8; --op: #8fb3ec; --n: #e3a271; --s: #d29be4; --c: #84918b; --l: #ee8fa4; --m: #84918b;
  color-scheme: dark; } }
:root[data-theme="dark"] {
  --bg: #121614; --surface: #181d1b; --fg: #e3e8e5; --muted: #9aa6a0; --rule: #2a322e;
  --accent: #6cc4a8; --code-bg: #1b2120; --red-bg: #19201c; --rc-bg: #181e23;
  --k: #6cc4a8; --op: #8fb3ec; --n: #e3a271; --s: #d29be4; --c: #84918b; --l: #ee8fa4; --m: #84918b;
  color-scheme: dark; }
* { box-sizing: border-box; }
body { background: var(--bg); color: var(--fg); font: 15.5px/1.6 var(--body); padding-inline: 16px; }
.layout { display: grid; grid-template-columns: 15rem minmax(0, 1fr); gap: 3rem; max-width: 74rem; margin: 0 auto; padding-block: 2rem 4rem; }
nav { position: sticky; top: calc(env(safe-area-inset-top, 0px) + 1.5rem); align-self: start; max-height: calc(100vh - 3rem); overflow-y: auto; font-size: 0.88rem; }
nav .brand { font-family: var(--display); font-weight: 600; font-size: 1.15rem; letter-spacing: 0.01em; margin: 0 0 1rem; }
nav ol { list-style: none; margin: 0; padding: 0; display: grid; gap: 0.35rem; }
nav ol ol { margin: 0.35rem 0 0.6rem 0.8rem; gap: 0.2rem; }
nav a { color: var(--muted); text-decoration: none; }
nav a:hover, nav a:focus-visible { color: var(--accent); }
nav > ol > li > a { color: var(--fg); font-weight: 500; }
main { min-width: 0; max-width: 52rem; }
header.intro { border-bottom: 1px solid var(--rule); padding-bottom: 1.5rem; margin-bottom: 1rem; }
header.intro .eyebrow { font-family: var(--mono); font-size: 0.78rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--accent); margin: 0; }
header.intro h1 { font-family: var(--display); font-weight: 600; font-size: clamp(2rem, 5vw, 2.8rem); line-height: 1.1; margin: 0.4rem 0 0.8rem; text-wrap: balance; }
header.intro p { color: var(--muted); max-width: 40rem; margin: 0; }
h2 { font-family: var(--display); font-weight: 600; font-size: 1.9rem; line-height: 1.2; margin: 3.5rem 0 1rem; padding-top: 1rem; border-top: 2px solid var(--fg); text-wrap: balance; }
h3 { font-family: var(--display); font-weight: 600; font-size: 1.3rem; margin: 2.2rem 0 0.6rem; text-wrap: balance; }
h4 { font-size: 1rem; margin: 1.6rem 0 0.4rem; }
p, li { max-width: 65ch; }
a { color: var(--accent); }
a:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; }
code { font-family: var(--mono); font-size: 0.88em; background: var(--code-bg); padding: 0.08em 0.3em; border-radius: 3px; }
.path { color: var(--muted); font-family: var(--mono); font-size: 0.85em; }
ul { padding-left: 1.2rem; }
li { margin: 0.25rem 0; }
.table { overflow-x: auto; margin: 1rem 0; }
table { border-collapse: collapse; font-size: 0.92rem; min-width: 100%; }
th, td { text-align: left; vertical-align: top; padding: 0.45rem 0.8rem 0.45rem 0; border-bottom: 1px solid var(--rule); }
th { font-family: var(--display); font-weight: 600; color: var(--muted); font-size: 0.85rem; letter-spacing: 0.02em; }
figure.code { margin: 1rem 0; min-width: 0; border: 1px solid var(--rule); border-radius: 6px; background: var(--code-bg); overflow: hidden; }
figure.code figcaption { font-family: var(--mono); font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--muted); padding: 0.45rem 0.8rem; border-bottom: 1px solid var(--rule); }
figure.red { background: var(--red-bg); }
figure.redcode { background: var(--rc-bg); }
pre { margin: 0; padding: 0.8rem; overflow-x: auto; font: 0.84rem/1.5 var(--mono); tab-size: 8; }
.pair { display: grid; grid-template-columns: minmax(0, 1fr) auto minmax(0, 1fr); gap: 0.6rem; align-items: start; margin: 1rem 0; }
.pair figure.code { margin: 0; }
.pair .arrow { align-self: center; color: var(--muted); font-family: var(--mono); }
.k { color: var(--k); font-weight: 600; } .op { color: var(--op); } .n { color: var(--n); } .s { color: var(--s); }
.c { color: var(--c); font-style: italic; } .l { color: var(--l); font-weight: 600; } .m { color: var(--m); }
footer { margin-top: 4rem; padding-top: 1rem; border-top: 1px solid var(--rule); color: var(--muted); font-size: 0.85rem; }
@media (max-width: 860px) {
  .layout { grid-template-columns: minmax(0, 1fr); gap: 1rem; padding-block: 1rem 3rem; }
  nav { position: static; max-height: none; border-bottom: 1px solid var(--rule); padding-bottom: 1rem; }
  nav ol ol { display: none; }
  .pair { grid-template-columns: minmax(0, 1fr); }
  .pair .arrow { transform: rotate(90deg); justify-self: center; }
}
@media (prefers-reduced-motion: no-preference) { html { scroll-behavior: smooth; } }
"""


def build() -> str:
    chapters = sorted(p for p in MANUAL.glob("*.md") if p.name != "README.md")
    index_html, _, _ = render((MANUAL / "README.md").read_text(), "README.md")
    # the index's own heading and title become the page's header
    index_body = re.sub(r"^<h2[^>]*>.*?</h2>\n", "", index_html)
    parts, nav = [], []
    for ch in chapters:
        body, title, sections = render(ch.read_text(), ch.name)
        parts.append(f'<section aria-labelledby="{chapter_id(ch.name)}">{body}</section>')
        subs = "".join(f'<li><a href="#{sid}">{html.escape(re.sub(r"^[0-9.]+ ", "", t))}</a></li>' for sid, t in sections)
        nav.append(f'<li><a href="#{chapter_id(ch.name)}">{html.escape(title)}</a><ol>{subs}</ol></li>')
    return f"""<title>RED Manual</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans+Condensed:wght@600&family=IBM+Plex+Sans:ital,wght@0,400;0,500;0,600;1,400&display=swap">
<style>{STYLE}</style>
<div class="layout">
<nav aria-label="Contents">
<p class="brand">RED manual</p>
<ol>{''.join(nav)}</ol>
</nav>
<main>
<header class="intro">
<p class="eyebrow">ICWS'94 redcode · pMARS · KotH hills</p>
<h1>Writing Core War warriors in RED</h1>
{index_body}
</header>
{''.join(parts)}
<footer>Built from <code>docs/manual/</code> by <code>tools/manual_page.py</code>. Every RED example on this page was compiled, and every redcode block is the compiler's output for it.</footer>
</main>
</div>
"""


def main(argv: list[str]) -> int:
    out = Path(argv[0]) if argv else ROOT / "_build" / "manual-page" / "index.html"
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(build())
    print(out)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
