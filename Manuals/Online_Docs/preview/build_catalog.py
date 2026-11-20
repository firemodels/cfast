#!/usr/bin/env python3
"""Generate the manuals home page from completed English builds."""
import argparse
import hashlib
import html
import json
from pathlib import Path
import shutil
import tempfile

from manuals import MANUALS

HERE = Path(__file__).resolve().parent


def write_atomic(path, content):
    with tempfile.NamedTemporaryFile(mode='w', encoding='utf-8', dir=path.parent, delete=False) as stream:
        stream.write(content)
        temporary = Path(stream.name)
    temporary.replace(path)


def catalog_entries(root):
    entries = []
    for manual in MANUALS.values():
        folder = root / manual.output / 'html/en'
        try:
            proof = json.loads((folder / 'build-manifest.json').read_text())
        except (OSError, ValueError):
            continue
        pages = proof.get('pages', [])
        if (proof.get('complete') is True and proof.get('language') == 'en'
                and proof.get('manual', 'tech') == manual.key and pages
                and (folder / 'index.html').is_file()
                and all(Path(p).name == p and (folder / p).is_file() for p in pages)):
            entries.append({'key': manual.key, 'title': manual.title, 'subtitle': manual.subtitle,
                            'url': f'{manual.output}/html/en/index.html'})
    return entries


def refresh_catalog(root):
    root = Path(root).resolve()
    root.mkdir(parents=True, exist_ok=True)
    destination = root / 'index.html'
    if destination.exists() and '<!-- CFAST manuals catalog -->' not in destination.read_text():
        raise RuntimeError(f'Refusing to replace an unrelated home page: {destination}')
    entries = catalog_entries(root)
    cards = []
    for entry in entries:
        link = f'<a href="{html.escape(entry["url"], quote=True)}">{html.escape(entry["title"])}</a>'
        cards.append(f'<section class="manual-card"><p class="volume">{html.escape(entry["subtitle"])}</p>'
                     f'<h3>{link}</h3></section>')
    cards = ('<section class="publication"><h2>NIST TN 1889</h2>'
             f'<div class="manual-grid">{"".join(cards)}</div></section>') if cards else ''
    content = (HERE / 'catalog.html').read_text().replace('{{CARDS}}', cards)
    content = content.replace('{{EMPTY}}', '' if entries else '<p>No manuals have completed building yet.</p>')
    shutil.copy2(HERE / 'catalog.css', root / 'catalog.css')
    digest = hashlib.sha256((HERE / 'catalog.css').read_bytes()).hexdigest()[:12]
    content = content.replace('catalog.css"', 'catalog.css?v=' + digest + '"')
    write_atomic(root / 'manuals.json', json.dumps({'kind': 'cfast-manual-catalog', 'manuals': entries}, indent=2))
    write_atomic(destination, content)
    (root / 'catalog.js').unlink(missing_ok=True)
    return entries


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=HERE.parent / 'build')
    args = parser.parse_args()
    entries = refresh_catalog(args.root)
    print(f'Manuals home: {args.root.resolve() / "index.html"} ({len(entries)} completed manuals)')
