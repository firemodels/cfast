"""Catalog publication checks without TeX, translation, or a browser."""
import json
from pathlib import Path
import tempfile
import unittest
from lxml import html
from build_catalog import catalog_entries, refresh_catalog


class CatalogTests(unittest.TestCase):
    def test_draft_notice_upgrades_existing_readers_without_duplicates(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            folder = self.edition(root, 'tech-guide', 'tech')
            (folder / 'assets').mkdir()
            page = folder / 'chapter.html'
            page.write_text('<html><head><script src="assets/reader.js"></script></head>'
                            '<body><header class="site-header"></header>'
                            '<article><h1 id="topic">Science</h1><math><mi>x</mi></math></article>'
                            '<footer class="page-footer"><span>Manual</span></footer></body></html>')
            for _ in range(2):
                refresh_catalog(root, update_readers=True)
            doc = html.parse(str(page))
            self.assertEqual(doc.xpath('//div[@class="draft-banner"]/text()'), ['DRAFT FOR REVIEW'])
            self.assertEqual(doc.xpath('//footer/strong/text()'), ['DRAFT FOR REVIEW'])
            self.assertEqual(doc.xpath('//article/h1/@id'), ['topic'])
            self.assertEqual(doc.xpath('//article/math/mi/text()'), ['x'])
            catalog = html.parse(str(root / 'index.html'))
            self.assertEqual(catalog.xpath('//div[@class="draft-banner"]/text()'), ['DRAFT FOR REVIEW'])

    def test_all_volumes_share_one_publication_in_volume_order(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            for key in ['config', 'validation', 'user', 'tech']:
                self.edition(root, key + '-guide', key)
            refresh_catalog(root)
            doc = html.parse(str(root / 'index.html'))
            self.assertEqual(doc.xpath('//section[@class="publication"]/h2/text()'), ['NIST TN 1889'])
            self.assertEqual(doc.xpath('//section/h3/a/@href'), [
                key + '-guide/html/en/index.html' for key in ['tech', 'user', 'validation', 'config']])

    def edition(self, root, output, key, language='en', complete=True):
        folder = root / output / 'html' / language
        folder.mkdir(parents=True)
        (folder / 'build-manifest.json').write_text(json.dumps({
            'manual': key, 'language': language, 'complete': complete,
            'pages': ['chapter.html']}))
        (folder / 'chapter.html').write_text('<html lang="en"><head><title>Chapter</title></head><body>Chapter</body></html>')
        (folder / 'index.html').write_text('Chapter')
        return folder

    def test_only_complete_correct_editions_with_present_pages(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            self.edition(root, 'tech-guide', 'tech')
            spanish = self.edition(root, 'tech-guide', 'tech', 'es')
            (spanish / 'chapter.html').unlink()
            self.edition(root, 'user-guide', 'user', complete=False)
            self.edition(root, 'config-guide', 'user')
            entries = catalog_entries(root)
            self.assertEqual([entry['key'] for entry in entries], ['tech'])
            self.assertEqual([e['code'] for e in entries[0]['editions']], ['en'])

    def test_generated_home_links_resolve_and_future_builds_join(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            self.edition(root, 'user-guide', 'user')
            refresh_catalog(root)
            doc = html.parse(str(root / 'index.html'))
            self.assertEqual(len(doc.xpath('//main/h1')), 1)
            self.assertEqual(doc.xpath('//section/h3/a/text()'), ['User Guide'])
            self.assertEqual(doc.xpath('//select[@id="catalog-language"]/option/@value'), ['en'])
            self.assertFalse(doc.xpath('//section/nav'))
            for href in doc.xpath('//section//a/@href'):
                self.assertTrue((root / href).is_file())
            self.edition(root, 'validation-guide', 'validation')
            refresh_catalog(root)
            data = json.loads((root / 'manuals.json').read_text())
            self.assertEqual([m['key'] for m in data['manuals']], ['user', 'validation'])
            self.assertTrue((root / 'catalog.js').is_file())

    def test_unrelated_index_is_preserved(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / 'index.html').write_text('Unrelated document')
            with self.assertRaisesRegex(RuntimeError, 'unrelated'):
                refresh_catalog(root)
            self.assertEqual((root / 'index.html').read_text(), 'Unrelated document')


if __name__ == '__main__':
    unittest.main()
