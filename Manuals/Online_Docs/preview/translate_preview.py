"""Protect scientific markup while extracting and applying translations."""
from collections import Counter
import hashlib
import html as escape_html
import re

from lxml import etree, html

TOKEN = re.compile(r'⟦(\d+)⟧')
BLOCKS = {'p', 'li', 'h1', 'h2', 'h3', 'h4', 'h5', 'figcaption'}


def text(element):
    return ' '.join(''.join(element.itertext()).split())


def translation_units(doc, include_tables=False):
    article = doc.xpath('//article')[0]
    units = []
    blocks = BLOCKS | ({'td', 'th'} if include_tables else set())
    for node in article.iterdescendants():
        if node.tag not in blocks or any(a.tag in blocks | {'math'} | (set() if include_tables else {'table'}) for a in node.iterancestors()):
            continue
        protected = []

        def protect(markup):
            token = f'⟦{len(protected)}⟧'
            protected.append(markup)
            return token

        def contents(parent):
            result = parent.text or ''
            for child in parent:
                if not isinstance(child.tag, str):
                    result += child.tail or ''
                    continue
                classes = child.get('class', '')
                if child.tag in {'math', 'a', 'img', 'table', 'div', 'code', 'br'} or 'pcr' in classes or 'titlemark' in classes:
                    result += protect(etree.tostring(child, encoding='unicode', method='html', with_tail=False))
                else:
                    opening = '<' + child.tag + ''.join(' ' + k + '="' + escape_html.escape(v, quote=True) + '"'
                                                       for k, v in child.attrib.items()) + '>'
                    result += protect(opening) + contents(child) + protect('</' + child.tag + '>')
                result += child.tail or ''
            return result

        source = ' '.join(contents(node).split())
        if not re.search(r'[A-Za-z]', TOKEN.sub('', source)):
            continue
        # The source digest deliberately excludes serialized math/formatting:
        # immutable placeholders are always restored from the current build.
        units.append({'id': f'b{len(units):03}', 'node': node, 'source': source,
                      'sha256': hashlib.sha256(source.encode()).hexdigest(), 'protected': protected})
    return units


def apply_translation(unit, translated):
    if Counter(TOKEN.findall(unit['source'])) != Counter(TOKEN.findall(translated)):
        raise ValueError(f"Protected placeholder mismatch: {unit['id']}")
    # Preserve protected-item order as well as exact markup.
    if TOKEN.findall(unit['source']) != TOKEN.findall(translated):
        raise ValueError(f"Protected placeholder order changed: {unit['id']}")
    markup = TOKEN.sub(lambda m: unit['protected'][int(m[1])], escape_html.escape(translated))
    replacement = html.fragment_fromstring(markup, create_parent=unit['node'].tag)
    node = unit['node']
    for child in list(node):
        node.remove(child)
    node.text = replacement.text
    node.extend(list(replacement))
