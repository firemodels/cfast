"""Identity and presentation metadata; scientific content stays in LaTeX."""
from dataclasses import dataclass


@dataclass(frozen=True)
class Manual:
    key: str
    directory: str
    output: str
    title: str
    subtitle: str
    title_es: str
    subtitle_es: str
    entry: str = 'chapter-1.html'

    @property
    def main(self):
        return self.directory + '.tex'

    @property
    def pdf(self):
        return self.directory + '.pdf'


MANUALS = {
    'tech': Manual('tech', 'CFAST_Tech_Ref', 'tech-guide',
                   'Technical Reference Guide', 'NIST TN 1889 · Volume 1',
                   'Guía de referencia técnica', 'NIST TN 1889 · Volumen 1'),
    'user': Manual('user', 'CFAST_Users_Guide', 'user-guide',
                   'User Guide', 'NIST TN 1889 · Volume 2',
                   'Guía del usuario', 'NIST TN 1889 · Volumen 2', 'getting-started.html'),
    'validation': Manual('validation', 'CFAST_Validation_Guide', 'validation-guide',
                         'Verification and Validation Guide', 'NIST TN 1889 · Volume 3',
                         'Guía de verificación y validación', 'NIST TN 1889 · Volumen 3', 'overview.html'),
    'config': Manual('config', 'CFAST_Configuration_Guide', 'config-guide',
                     'Configuration Management Guide', 'NIST TN 1889 · Volume 4',
                     'Guía de gestión de la configuración', 'NIST TN 1889 · Volumen 4'),
}

TECH = MANUALS['tech']
