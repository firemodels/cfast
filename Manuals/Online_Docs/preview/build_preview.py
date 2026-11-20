#!/usr/bin/env python3
"""Convert CFAST LaTeX with make4ht; --full --manual selects a complete manual.

This pilot requires an already built local Tech Guide (PDF and .aux files) for
links to chapters outside the preview. It does not fetch release assets.
"""
from __future__ import annotations

import argparse
import hashlib
import html
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time
from urllib.parse import quote, unquote, urlsplit

from accessibility import heading_levels, adapt_content
from manuals import MANUALS as MANUAL_SPECS, TECH

from lxml import etree
from lxml import html as dom

HERE = Path(__file__).resolve().parent
DOCS = HERE.parent
MANUALS = DOCS.parent
GUIDE = MANUALS / TECH.directory


def run(args, cwd, log=None, progress=False):
    env = os.environ.copy()
    env['PATH'] = '/Library/TeX/texbin:/opt/homebrew/bin:' + env['PATH']
    env['TEXINPUTS'] = '.:../LaTeX_Style_Files:'
    env['texmf_casefold_search'] = '0'
    env['XDG_CACHE_HOME'] = str(Path(cwd) / '.cache')
    # MacTeX does not discover Homebrew's Ghostscript library automatically.
    if 'LIBGS' not in env and Path('/opt/homebrew/lib/libgs.dylib').exists():
        env['LIBGS'] = '/opt/homebrew/lib/libgs.dylib'
    if log:
        with log.open('w') as stream:
            if not progress:
                subprocess.run(args, cwd=cwd, env=env, stdout=stream,
                               stderr=subprocess.STDOUT, check=True)
            else:
                started = last_update = time.monotonic()
                # Keep the complete log on disk while showing selected live
                # milestones. File polling also works when TeX is quiet.
                with log.open() as updates, subprocess.Popen(
                        args, cwd=cwd, env=env, stdout=stream,
                        stderr=subprocess.STDOUT) as process:
                    while True:
                        finished = process.poll() is not None
                        for line in updates:
                            if '[CFAST-DOCS]' in line:
                                print(line.split('[CFAST-DOCS]', 1)[1].strip(), flush=True)
                                last_update = time.monotonic()
                        now = time.monotonic()
                        if finished:
                            break
                        if now - last_update >= 30:
                            print(f'  Conversion running: {int(now - started)} seconds elapsed; '
                                  'details in conversion.log', flush=True)
                            last_update = now
                        time.sleep(0.25)
                    if process.returncode:
                        raise subprocess.CalledProcessError(process.returncode, args)
    else:
        return subprocess.check_output(args, cwd=cwd, env=env, text=True).strip()


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def group(text, pos):
    """Read a brace-balanced TeX argument (used only for .aux metadata)."""
    if text[pos] != '{':
        raise ValueError(f'Expected brace at {pos}')
    start, depth = pos + 1, 1
    for i in range(start, len(text)):
        if text[i] in '{}' and (i == 0 or text[i - 1] != '\\'):
            depth += 1 if text[i] == '{' else -1
            if depth == 0:
                return text[start:i], i + 1
    raise ValueError('Unclosed TeX argument')


def group_math_scripts(text):
    """Give TeX4ht an explicit group around font/text commands in scripts."""
    pattern = re.compile(r'([_^])\s*\\(?:text(?:rm|sf|tt|bf|it|normal)?|math(?:rm|sf|tt|bf|it|cal|bb|normal)|mbox|hbox)\s*(?=\{)')
    parts, pos = [], 0
    while match := pattern.search(text, pos):
        _, end = group(text, match.end())
        parts.append(text[pos:match.start()])
        parts.append(match[1] + '{' + group_math_scripts(text[match.start() + 1:end].strip()) + '}')
        pos = end
    parts.append(text[pos:])
    return ''.join(parts)


def group_compound_math(text):
    """Brace compound TeX atoms for TeX4ht without changing their operands."""
    def atom_end(pos):
        while pos < len(text) and text[pos].isspace():
            pos += 1
        if pos == len(text):
            raise ValueError('Missing math operand')
        if text[pos] == '{':
            return group(text, pos)[1]
        command = re.match(r'\\([A-Za-z]+|.)', text[pos:])
        if not command:
            return pos + 1
        end = pos + len(command[0])
        if command[1] in {'frac', 'dfrac', 'tfrac'}:
            return atom_end(atom_end(end))
        if command[1] == 'sqrt':
            option = re.match(r'\s*\[[^\]]*\]', text[end:])
            if option:
                end += len(option[0])
            return atom_end(end)
        return end

    pattern = re.compile(r'(?P<prefix>[_^]|\\sqrt(?:\s*\[[^\]]*\])?)\s*(?=\\(?:frac|dfrac|tfrac|sqrt)\b)')
    parts, pos = [], 0
    while match := pattern.search(text, pos):
        end = atom_end(match.end())
        parts.extend([text[pos:match.start()], match['prefix'], '{',
                      group_compound_math(text[match.end():end]), '}'])
        pos = end
    parts.append(text[pos:])
    return ''.join(parts)


def prepare_math(text):
    # Literal code examples are content, not TeX math to be normalized.
    literal = re.compile(r'(\\begin\{(?:lstlisting|verbatim)\}.*?\\end\{(?:lstlisting|verbatim)\}|\\verb\*?(.).*?\2)', re.S)
    parts, pos = [], 0
    for match in literal.finditer(text):
        parts.extend([group_nested_primes(group_math_scripts(group_compound_math(text[pos:match.start()]))), match[0]])
        pos = match.end()
    parts.append(group_nested_primes(group_math_scripts(group_compound_math(text[pos:]))))
    return ''.join(parts)


def group_nested_primes(text):
    """Avoid TeX4ht recursion when primes follow a primed, grouped subscript."""
    parts, pos = [], 0
    for match in re.finditer(r'(?<!\\)_\s*(?=\{)', text):
        if match.start() < pos:
            continue
        argument, end = group(text, match.end())
        primes = re.match(r"'+", text[end:])
        if "'" not in argument or not primes:
            continue
        stop = end + len(primes[0])
        # TeX combines x'^n into one superscript; leave that distinct case
        # alone rather than accidentally creating a second superscript.
        if text[stop:].lstrip().startswith('^'):
            continue
        parts.append(text[pos:end])
        parts.append('^{' + r'\prime' * len(primes[0]) + '}')
        pos = stop
    parts.append(text[pos:])
    return ''.join(parts)


def read_labels(guide=GUIDE):
    result = {}
    for path in guide.glob('*.aux'):
        for line in path.read_text().splitlines():
            if not line.startswith(r'\newlabel{'):
                continue
            key, end = group(line, len(r'\newlabel'))
            value, _ = group(line, end)
            fields, pos = [], 0
            while pos < len(value) and value[pos] == '{':
                field, pos = group(value, pos)
                fields.append(field)
            if len(fields) >= 4:
                result[key] = {'number': fields[0], 'page': fields[1], 'destination': fields[3]}
    return result


def plain(element):
    return ' '.join(''.join(element.itertext()).split())


def normalize_math(body):
    """Preserve differential operators and clearly separated math accents."""
    align_equation_numbers(body)
    # MathML Core does not inherit legacy mstyle mathvariant onto single-letter
    # mi tokens. Preserve explicit LaTeX roman type on the token itself (SP 811,
    # 10.2.3), including differential d/D, units, and descriptive subscripts.
    for symbol in body.xpath('.//math//mi[not(@mathvariant)]'):
        styles = symbol.xpath('ancestor::*[@mathvariant]')
        if styles and styles[-1].get('mathvariant') == 'normal':
            symbol.set('mathvariant', 'normal')

    # Audited unmarked integral measures in 2.15 and 3.29: d followed by a
    # primed integration variable at the end of the integral's row. Never
    # globally romanize d: it also denotes nozzle diameter in Chapter 9.
    for symbol in body.xpath('.//math//mi[text()="d"][not(@mathvariant)]'):
        previous = symbol.getprevious()
        following = symbol.getnext()
        if (previous is not None and previous.tag == 'mspace'
                and following is not None and following.tag == 'msup'
                and following.getnext() is None
                and any('∫' in plain(n) for n in symbol.itersiblings(preceding=True))):
            symbol.set('mathvariant', 'normal')

    for nabla in body.xpath('.//math//*[self::mi or self::mo][text()="∇"]'):
        following = [node for node in nabla.itersiblings() if isinstance(node.tag, str)]
        application, negative_spaces, operator = [], [], None
        for node in following:
            if node.tag == 'mo' and plain(node) == '\u2061':
                # Nabla is not a named function such as sin. The inserted
                # application token also damages gradients and Laplacians.
                application.append(node)
            elif node.tag == 'mspace' and node.get('class') == 'negthinspace':
                negative_spaces.append(node)
            else:
                if node.tag == 'mo' and plain(node) in {'⋅', '×'}:
                    operator = node
                break
        for node in application:
            node.getparent().remove(node)
        nabla.tag = 'mo'
        nabla.attrib.pop('class', None)
        nabla.set('lspace', '0em')
        nabla.set('rspace', '0em')
        if operator is not None:
            if negative_spaces:
                for node in negative_spaces:
                    node.getparent().remove(node)
                # A TeX binary operator gets 4mu; \! removes 3mu on its left.
                operator.set('lspace', '0.0556em')
            elif operator.get('lspace') is None:
                operator.set('lspace', '0.2222em')
            operator.set('rspace', '0.2222em')

    for symbol in body.xpath('.//math//mi[text()="ṁ"]'):
        # A precomposed Latin extended character may use an upright fallback
        # glyph. A native accent over the mathematical m preserves italics.
        accent = etree.Element('mover', accent='true')
        base = etree.SubElement(accent, 'mi', dict(symbol.attrib))
        base.text = 'm'
        mark = etree.SubElement(accent, 'mo', accent='true', stretchy='false')
        mark.text = '˙'
        accent.tail = symbol.tail
        symbol.getparent().replace(symbol, accent)

    for accent in body.xpath('.//math//mover'):
        children = [node for node in accent if isinstance(node.tag, str)]
        if len(children) != 2:
            continue
        mark = children[1]
        # Remove the earlier lift, including when normalizing an existing tree.
        # Wrapping an accent in mpadded changes its layout as well as shifting
        # it, so reducing voffset alone did not fix the floating dots in 3.6.
        if mark.tag == 'mpadded' and mark.get('class') == 'dot-accent':
            wrapper = mark
            mark = wrapper[0]
            mark.tail = wrapper.tail
            accent.replace(wrapper, mark)
        if mark.tag != 'mo' or plain(mark) not in {'˙', '\u0307'}:
            continue
        # A direct accent token lets the font supply placement and attachment.
        accent.set('accent', 'true')
        mark.set('accent', 'true')
        mark.set('stretchy', 'false')
        base = children[0]
        if (base.tag == 'mrow' and not base.attrib and len(base) == 1
                and not (base.text or '').strip() and not (base[0].tail or '').strip()):
            symbol = base[0]
            symbol.tail = base.tail
            accent.replace(base, symbol)

    # A stretchy accent still rendered as a short glyph in the user's browser.
    # Draw every LaTeX \overline as a rule spanning its entire MathML base.
    # Process inner lines first so nested overlines retain both rules.
    for accent in reversed(body.xpath('.//math//mover')):
        children = [node for node in accent if isinstance(node.tag, str)]
        if (len(children) == 2
                and 'mml-overline' in accent.get('class', '').split()
                and children[1].tag == 'mo' and plain(children[1]) in {'¯', '‾', '\u0305'}):
            overline = etree.Element('mrow', dict(accent.attrib))
            overline.attrib.pop('accent', None)
            base_text = plain(children[0])
            overline.set('class', 'mixture-overline' if base_text == 'W' else 'latex-overline')
            overline.set('aria-label', base_text + ' with overline')
            overline.append(children[0])
            overline.tail = accent.tail
            accent.getparent().replace(accent, overline)


def align_equation_numbers(body):
    """Mark explicit equation labels in native MathML, independent of row layout.

    Retain one native MathML table so tall fractions and multiline operands
    determine both expression and label row heights. The reader positions visual
    labels from those rendered rows without splitting the native math nodes.
    """
    for table in body.xpath('.//math[@display="block"]//mtable'):
        classes = table.get('class', '').split()
        if 'numbered-equations' in classes:
            continue
        labels = []
        for marker in table.xpath('.//mstyle[contains(concat(" ",normalize-space(@class)," ")," maketag ")] | '
                                  './/mtext[contains(concat(" ",normalize-space(@class)," ")," eqnarray ")]'):
            if marker.xpath('ancestor::mtable')[-1] is not table or not plain(marker):
                continue
            cell = marker.xpath('ancestor::mtd')[-1]
            if cell not in labels:
                labels.append(cell)
        if not labels:
            continue
        table.set('class', ' '.join(classes + ['numbered-equations']))
        math = table.xpath('ancestor::math')[0]
        math.set('class', (math.get('class', '') + ' numbered-math').strip())
        for label in labels:
            label.set('class', (label.get('class', '') + ' equation-label').strip())


def reader_link(href, current, filename_map, id_files):
    """Resolve local anchors against their actual generated page, including citations."""
    # TeX4ht's id filter escapes '=' even in external PDF destination parameters.
    if href.startswith('external-manuals/'):
        href = href.replace('.pdf#nameddest_', '.pdf#nameddest=', 1)
    url = urlsplit(href)
    if url.scheme or url.netloc:
        return href
    path, sep, fragment = href.partition('#')
    if path == 'preview.html' and fragment in filename_map:
        return filename_map[fragment]
    if not path or path in filename_map:
        destinations = id_files.get(fragment, [])
        if len(destinations) == 1:
            return (destinations[0] if destinations[0] != current else '') + '#' + fragment
        if path in filename_map:
            return filename_map[path] + (sep + fragment if sep else '')
    return href


def missing_reference_link(link, href, labels, id_files, filename_map):
    """Recover omitted TeX4ht label anchors using the authoritative PDF metadata."""
    url = urlsplit(href)
    if (url.scheme or url.netloc or not url.fragment or url.fragment in id_files
            or (url.path and url.path not in filename_map)):
        return href
    for comment in link.xpath('./comment()'):
        match = re.fullmatch(r'\s*tex4ht:ref:\s*(.+?)\s*', comment.text or '')
        target = labels.get(match[1]) if match else None
        if target and target['destination'] and '\\' not in target['number']:
            for child in list(link):
                link.remove(child)
            link.text = target['number']
            return 'full-guide.pdf#nameddest=' + target['destination']
    return href


def restore_reference_number(link, labels):
    for comment in link.xpath('./comment()'):
        match = re.fullmatch(r'\s*tex4ht:ref:\s*(.+?)\s*', comment.text or '')
        target = labels.get(match[1]) if match else None
        if target and target['number'] and '\\' not in target['number']:
            for child in list(link):
                if not isinstance(child, etree._Comment):
                    link.remove(child)
                else:
                    child.tail = None
            link.text = target['number']
            return


def make_reader(tex, site, fallbacks, pages=None, full=False, manual=TECH):
    guide = MANUALS / manual.directory
    site.mkdir(parents=True, exist_ok=True)
    assets = site / 'assets'
    assets.mkdir(exist_ok=True)
    for name in ['reader.css', 'reader.js']:
        shutil.copy2(HERE / name, assets / name)
    shutil.copy2(tex / 'preview.css', assets / 'tex4ht.css')
    font = Path(run(['kpsewhich', 'latinmodern-math.otf'], tex))
    if font.is_file():
        shutil.copy2(font, assets / 'latinmodern-math.otf')
    shutil.copy2(guide / manual.pdf, site / 'full-guide.pdf')
    if (tex / 'external-manuals').is_dir():
        shutil.copytree(tex / 'external-manuals', site / 'external-manuals', dirs_exist_ok=True)
    filename_map = {'preview.html': 'index.html'}
    if pages is None:
        raise ValueError('Complete manual page inventory is required')
    filename_map.update({page['raw']: page['file'] for page in pages})
    pdf_labels = read_labels(guide) if full else {}
    figure_map_path = tex / 'web-figures.json'
    figure_map = json.loads(figure_map_path.read_text()) if figure_map_path.exists() else {}

    id_files = {}
    for position, page in enumerate(pages, 1):
        if full:
            print(f'Reading page {position}/{len(pages)}: {page["file"]}', flush=True)
        tree = dom.parse(str(tex / page['raw']), parser=dom.HTMLParser(encoding='utf-8'))
        body = tree.find('body')
        for element in body.xpath('.//div[@class="crosslinks"] | .//div[@class="crosslinks-bottom"]'):
            element.drop_tree()
        headings = body.xpath('./h1 | ./h2')
        if not headings and page.get('synthetic_title'):
            heading = etree.Element('h2', id='manual-index')
            heading.text = page['synthetic_title']
            body.insert(0, heading)
        else:
            heading = headings[0]
        marker = heading.find('span')
        if marker is not None:
            marker.drop_tree()
        for br in heading.findall('br'):
            br.drop_tree()
        heading.tag = 'h1'
        heading_levels(body)
        page['title'] = plain(heading)
        page['body'] = body
        page['toc'] = [(element.get('id'), plain(element), element.tag == 'h3')
                       for element in body.xpath('.//h2[@id] | .//h3[@id]')]
        for element in body.xpath('.//*[@id]'):
            # Shared empty bibliography helper anchors are not destinations.
            if element.get('id') == 'X0-':
                element.attrib.pop('id')
            else:
                id_files.setdefault(element.get('id'), []).append(page['file'])

    for page in pages:
        body = page['body']
        normalize_math(body)
        for link in list(body.iter()):
            key = next((key for key in ['href', 'xlink:href', '{http://www.w3.org/1999/xlink}href']
                        if link.get(key) is not None), None)
            if key is None:
                continue
            href = link.get(key)
            restore_reference_number(link, pdf_labels)
            href = missing_reference_link(link, href, pdf_labels, id_files, filename_map)
            link.set(key, reader_link(href, page['file'], filename_map, id_files))
            if 'full-guide.pdf' in href or href.startswith('external-manuals/'):
                link.set('title', 'Open this reference in the full PDF manual')
                link.set('class', 'pdf-reference')
        for image in body.xpath('.//img'):
            original = tex / image.get('src')
            if image.get('src') in figure_map:
                original = tex / figure_map[image.get('src')]
            elif original.stem == 'particle_size_distribution':
                original = original.with_suffix('.svg')
            if not original.is_file():
                raise RuntimeError(f'Missing converted image: {original}')
            shutil.copy2(original, assets / original.name)
            image.set('src', 'assets/' + original.name)
            image.set('loading', 'lazy')
            if image.get('alt'):
                continue
            image.set('alt', 'Technical diagram')
        adapt_content(body)

    from full_guide import input_signature, inventory
    signature = input_signature(inventory(manual))
    revision = 'sha256:' + hashlib.sha256(json.dumps(signature, sort_keys=True).encode()).hexdigest()[:12]
    template = (HERE / 'reader.html').read_text()
    if full:
        template = template.replace('IN THIS PREVIEW', 'IN THIS MANUAL').replace(
            'Selected chapters from the<br>CFAST {{MANUAL_TITLE}}.',
            'Complete manual.')
        template = template.replace('Search the chapters included in this preview.', 'Search this manual.')
    # Refresh presentation assets automatically when their contents change.
    for name in ['reader.css', 'reader.js']:
        template = template.replace(f'assets/{name}', f'assets/{name}?v={sha(HERE / name)[:12]}')
    search = []
    current_page = 'aria-current="page"'
    for position, page in enumerate(pages):
        if full:
            print(f'Writing page {position + 1}/{len(pages)}: {page["file"]}', flush=True)
        nav = ''.join(
            f'<a class="chapter-link {"active" if p is page else ""}" '
            f'{current_page if p is page else ""} href="{p["file"]}">'
            f'<span class="chapter-number">{p["number"] or ""}</span>'
            f'<span>{html.escape(p["title"])}</span></a>' for p in pages)
        toc = ''.join(f'<a class="{"subsection" if sub else ""}" href="#{anchor}">{html.escape(title)}</a>'
                      for anchor, title, sub in page['toc'])
        content = ''.join(etree.tostring(c, method='html', encoding='unicode') for c in page['body'])
        prev = pages[position - 1] if position > 0 else None
        nxt = pages[position + 1] if position + 1 < len(pages) else None
        pager = ''.join(f'<a href="{p["file"]}" class="{direction}"><small>{label}</small>{html.escape(p["title"])}</a>'
                        for p, direction, label in [(prev, 'previous', 'Previous'), (nxt, 'next', 'Next')]
                        if p is not None)
        if page['source']:
            source = assets / Path(page['source']).name
            shutil.copy2(guide / page['source'], source)
            source_link = f'<a href="assets/{source.name}">View LaTeX source ↗</a>'
        else:
            source_link = ''
        values = {'MANUAL_TITLE': html.escape(manual.title),
                  'MANUAL_SUBTITLE': html.escape(manual.subtitle), 'MANUAL_ENTRY': manual.entry,
                  'TITLE': html.escape(page['title']), 'NAV': nav, 'TOC': toc,
                  'CONTENT': content, 'PAGER': pager, 'REVISION': revision,
                  'EYEBROW': page.get('eyebrow', 'CHAPTER ' + str(page['number']) if page['number'] else 'BIBLIOGRAPHY'),
                  'SOURCE': source_link}
        output = template
        for key, value in values.items():
            output = output.replace('{{' + key + '}}', value)
        (site / page['file']).write_text(output)

        title, anchor, texts = page['title'], '', []
        for element in page['body']:
            if element.tag in {'h1', 'h2', 'h3'}:
                if texts:
                    search.append({'title': title, 'chapter': page['title'],
                                   'url': page['file'] + ('#' + anchor if anchor else ''),
                                   'text': ' '.join(texts)})
                title, anchor, texts = plain(element), element.get('id', ''), []
            elif isinstance(element.tag, str):
                texts.append(plain(element))
        if texts:
            search.append({'title': title, 'chapter': page['title'],
                           'url': page['file'] + ('#' + anchor if anchor else ''), 'text': ' '.join(texts)})
    (site / 'index.html').write_text('<!doctype html><html lang="en"><meta charset="utf-8">'
        f'<meta http-equiv="refresh" content="0;url={manual.entry}"><title>CFAST {html.escape(manual.title)}</title>'
        f'<a href="{manual.entry}">Open the CFAST {html.escape(manual.title)}</a></html>')
    (site / 'search-index.json').write_text(json.dumps(search, ensure_ascii=False))
    return pages, revision


def validate(site, pages, progress=False):
    docs = {p['file']: dom.parse(str(site / p['file'])) for p in pages}
    ids = {name: set(doc.xpath('//@id')) for name, doc in docs.items()}
    errors = []
    for position, (name, doc) in enumerate(docs.items(), 1):
        if progress:
            print(f'Validating page {position}/{len(docs)}: {name}', flush=True)
        # Validate before HTML's error recovery can conceal malformed math.
        for math in re.findall(r'<math\b.*?</math>', (site / name).read_text(), re.S):
            try:
                node = etree.fromstring(math.encode())
                if node.xpath('.//math'):
                    errors.append(f'{name}: nested math elements')
            except etree.XMLSyntaxError as error:
                errors.append(f'{name}: malformed MathML: {error}')
        for src in doc.xpath('//img/@src | //script/@src | //link[@rel="stylesheet"]/@href'):
            if not (site / urlsplit(src).path).is_file():
                errors.append(f'{name}: missing asset {src}')
        for href in doc.xpath('//a/@href'):
            url = urlsplit(href)
            if url.scheme or url.netloc:
                continue
            target = unquote(url.path) or name
            if not (site / target).is_file():
                errors.append(f'{name}: missing page {href}')
            if url.fragment and target in ids and unquote(url.fragment) not in ids[target]:
                errors.append(f'{name}: missing anchor {href}')
        if '??' in ''.join(doc.xpath('//article//text()')):
            errors.append(f'{name}: unresolved LaTeX reference')
    if errors:
        raise RuntimeError('\n'.join(errors))
    return {name: {'math': len(doc.xpath('//math')), 'figures': len(doc.xpath('//article//figure')),
                   'equations': len(doc.xpath('//td[@class="eq-no"]'))} for name, doc in docs.items()}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, help='Build root; defaults to a separate directory per manual')
    parser.add_argument('--full', action='store_true', help=argparse.SUPPRESS)
    parser.add_argument('--manual', choices=list(MANUAL_SPECS), default='tech')
    parser.add_argument('--check', action='store_true', help='Check inputs without converting or writing output')
    parser.add_argument('--reuse-conversion', action='store_true', help='Restyle unchanged converted HTML')
    from full_guide import build_full
    build_full(parser.parse_args())


if __name__ == '__main__':
    main()
