"""
Usage:

>>> from fundep import *

# Funktionale Abhängigkeiten parsen
# Parser ist recht großzügig: Es kann → oder -> verwendet werden,
# Attribute müssen Großbuchstaben sein, alles sonstige (wie z.B. die Zahlen im Beispiel)
# wird ignoriert.
>>> d = parse_deps('''
... A → BC (1)
... AC → DE (2)
... AG → E (3)
... BD → A (4)
... EG → D
... ''')
>>> print(d)
[A→BC, AC→DE, AG→E, BD→A, EG→D]

# Kanonische Überdeckung berechnen
>>> canonical_cover(d)
Linksreduktion:
- A→BC ist nicht reduzierbar.
- AC→DE ist reduzierbar um C zu A→DE.
- A→DE ist nicht reduzierbar.
- AG→E ist reduzierbar um G zu A→E.
- A→E ist nicht reduzierbar.
- BD→A ist nicht reduzierbar.
- EG→D ist nicht reduzierbar.
Rechtsreduktion:
- A→BC ist nicht reduzierbar.
- A→DE ist reduzierbar um E zu A→D.
- A→D ist nicht reduzierbar.
- A→E ist nicht reduzierbar.
- BD→A ist nicht reduzierbar.
- EG→D ist nicht reduzierbar.
Entferne leere Abhaengigkeiten:
Zusammenfassung der Abhaengigkeiten:
- A→BC, A→D, A→E wird zusammengefasst zu A→BCDE
[A→BCDE, BD→A, EG→D]

# Attributhülle berechnen
>>> closure('A', d)
ABCDE

# Schlüsselkandidaten finden. (Erstes Argument ist die Menge aller Attribute).
>>> find_keys('ABCDEG', d)
[AG, BEG, BDG]


# Dekompositionsalgorithmus (Erstes Argument ist die Menge aller Attribute).
>>> decompose('ABCDEG', d)
Beginne mit R := ({A, B, C, D, E, G}, [A→BC, AC→DE, AG→E, BD→A, EG→D])
R_ := ({A, B, C, D, E, G}, [A→BC, AC→DE, AG→E, BD→A, EG→D]) ist nicht in BCNF, aufgrund von A→BC, da A kein Superschlüssel ist.
Zerlege in:
- R_1 := ({A, B, C}, [A→BC])
- R_2 := ({A, D, E, G}, [A→DE, EG→D])
R_2 := ({A, D, E, G}, [A→DE, EG→D]) ist nicht in BCNF, aufgrund von A→DE, da A kein Superschlüssel ist.
Zerlege in:
- R_21 := ({A, D, E}, [A→DE])
- R_22 := ({A, G}, [])
[(ABC, [A→BC]), (ADE, [A→DE]), (AG, [])]

# Synthesealgorithmus (Erstes Argument ist die Menge aller Attribute).
>>> synthesize('ABCDEG', d)
Bilden der kanonischen Überdeckung:
Linksreduktion:
- A→BC ist nicht reduzierbar.
- AC→DE ist reduzierbar um C zu A→DE.
- A→DE ist nicht reduzierbar.
- AG→E ist reduzierbar um G zu A→E.
- A→E ist nicht reduzierbar.
- BD→A ist nicht reduzierbar.
- EG→D ist nicht reduzierbar.
Rechtsreduktion:
- A→BC ist nicht reduzierbar.
- A→DE ist reduzierbar um E zu A→D.
- A→D ist nicht reduzierbar.
- A→E ist nicht reduzierbar.
- BD→A ist nicht reduzierbar.
- EG→D ist nicht reduzierbar.
Entferne leere Abhaengigkeiten:
Zusammenfassung der Abhaengigkeiten:
- A→BC, A→D, A→E wird zusammengefasst zu A→BCDE
Zwischenergebnis: [A→BCDE, BD→A, EG→D]
Relationenschemata bilden:
- Aus A→BCDE bilde eine Relation R_ABCDE := ({A, B, C, D, E}, [A→BCDE, BD→A])
- Aus BD→A bilde eine Relation R_BDA := ({A, B, D}, [BD→A])
- Aus EG→D bilde eine Relation R_EGD := ({D, E, G}, [EG→D])
Keine Relation enthält einen Schlüsselkandidaten.
Erzeuge aus Schlüsselkandidat {A, G} neue Relation R_AG := ({A, G}, [AG→AG])
Die Attribute von R_BDA sind in R_ABCDE enthalten; Entferne R_BDA
Endergebnis:
 - R_ABCDE := ({A, B, C, D, E}, [A→BCDE, BD→A])
 - R_EGD := ({D, E, G}, [EG→D])
 - R_AG := ({A, G}, [AG→AG])
[(ABCDE, [A→BCDE, BD→A]), (DEG, [EG→D]), (AG, [AG→AG])]

"""

from collections import namedtuple
import re
from string import ascii_uppercase
import itertools

Dep = namedtuple('Dep', ['left', 'right'])


def dep_to_str(d):
    return '{}→{}'.format(''.join(sorted(d.left)) or '∅',
                          ''.join(sorted(d.right)) or '∅')


Dep.__repr__ = dep_to_str

class Attr(frozenset):
    def __repr__(self):
        return ''.join(sorted(self))


def parse_deps(s):
    return [
        Dep(
            frozenset(left).intersection(ascii_uppercase),
            frozenset(right).intersection(ascii_uppercase))
        for left, right in re.findall(r'([A-Z\s]+)\s*(?:→|->)\s*([A-Z\s]+)', s)
    ]


def closure(attributes, deps):
    values = set(attributes)
    unused = set(deps)
    while unused:
        for dep in unused:
            if dep.left <= values:
                values.update(dep.right)
                unused.remove(dep)
                break
        else:
            return Attr(values)
    return Attr(values)


def is_super_key(candidate, attributes, deps):
    return set(attributes) <= closure(candidate, deps)


def is_key(candidate, attributes, deps):
    return is_super_key(candidate, attributes, deps) and not any(
        is_super_key(candidate - {a}, attributes, deps) for a in candidate)


def find_keys(relation, deps):
    def rec(attributes):
        if is_super_key(attributes, relation, deps):
            key = frozenset.union(
                *[rec(attributes - frozenset({x}))
                  for x in attributes]) or frozenset({attributes})
            return Attr(key)
        else:
            return frozenset()

    return [Attr(key) for key in rec(frozenset(relation))]


def canonical_cover(deps, silent=False):
    remaining = deps.copy()
    left_reduced = []
    if not silent:
        print('Linksreduktion:')
    while remaining:
        d = remaining[0]
        # print('Betrachte {}'.format(d))
        left, right = d
        for a in sorted(left):
            # print('Hülle ohne {}: {}'.format(a, closure(left - {a}, remaining + final)))
            if right <= closure(left - {a}, remaining + left_reduced):
                new_dep = Dep(left - {a}, right)
                if not silent:
                    print('- {} ist reduzierbar um {} zu {}.'.format(
                        d, a, new_dep))
                remaining.insert(1, new_dep)
                break
        else:
            # Non-reducible
            if not silent:
                print('- {} ist nicht reduzierbar.'.format(d))
            left_reduced.append(d)
        remaining.pop(0)

    remaining = left_reduced
    right_reduced = []
    if not silent:
        print('Rechtsreduktion:')
    while remaining:
        d = remaining.pop(0)
        left, right = d
        for b in sorted(right):
            if b in closure(
                    left,
                    remaining + right_reduced + [Dep(left, right - {b})]):
                new_dep = Dep(left, right - {b})
                if not silent:
                    print('- {} ist reduzierbar um {} zu {}.'.format(
                        d, b, new_dep))
                remaining.insert(0, new_dep)
                break
        else:
            if not silent:
                print('- {} ist nicht reduzierbar.'.format(d))
            right_reduced.append(d)

    if not silent:
        print('Entferne leere Abhaengigkeiten:')
    remaining = []
    for dep in right_reduced:
        if dep.right != set():
            remaining.append(dep)
        else:
            if not silent:
                print('- {} geloescht.'.format(dep))
    final = []
    if not silent:
        print('Zusammenfassung der Abhaengigkeiten:')
    while remaining:
        left, right = remaining.pop(0)
        new_right = right
        output = '- {}, '.format(Dep(left, right))
        for dep in remaining.copy():
            if dep.left == left:
                new_right = new_right | dep.right
                output += '{}, '.format(dep)
                remaining.remove(dep)
        final.append(Dep(left, new_right))
        if right != new_right:
            output = output[:-2] + ' wird zusammengefasst zu {}'.format(Dep(left, new_right))
            if not silent:
                print(output)
    return final


Rel = namedtuple('Rel', ['name', 'attrs', 'deps'])

def synthesize(relation, deps):
    # Step 1
    print('Bilden der kanonischen Überdeckung:')
    fc = canonical_cover(deps)
    print('Zwischenergebnis: {}\n'.format(fc))

    # Step 2
    print('Relationenschemata bilden:')
    rels = []
    for dep in fc:
        left, right = dep
        name = ''.join(itertools.chain(sorted(left), sorted(right)))
        attrs = left | right
        fs = [f for f in fc if (f.left | f.right) <= attrs]
        print('- Aus {} bilde eine Relation R_{} := ({{{}}}, {})'.format(
            dep, name, ', '.join(sorted(attrs)), fs))
        rels.append(Rel(name, attrs, fs))
    print()

    # Step 3
    keys = find_keys(relation, deps)
    keys.sort(key=lambda k: ''.join(sorted(k)))
    if not any(key <= r.attrs for r in rels for key in keys):
        print('Keine Relation enthält einen Schlüsselkandidaten.')
        key = keys[0]
        name = ''.join(sorted(key))
        deps = [Dep(key, key)]
        attr_str = ', '.join(sorted(key))
        print(
            'Erzeuge aus Schlüsselkandidat {{{}}} neue Relation R_{} := ({{{}}}, {})'
            .format(attr_str, name, attr_str, deps))
        rels.append(Rel(name, key, deps))
    print()

    # Step 4
    for rel in rels.copy():
        name, a, d = rel
        conflict = next(
            (oname
             for oname, other, _ in rels if oname != name and a <= other),
            None)
        if conflict is not None:
            print(
                'Die Attribute von R_{} sind in R_{} enthalten; Entferne R_{}'.
                format(name, conflict, name))
            rels.remove(rel)
    print()

    print('Endergebnis:')
    for name, attr, deps in rels:
        print(' - R_{} := ({{{}}}, {})'.format(name, ', '.join(sorted(attr)),
                                               deps))
    return [(Attr(attr), deps) for (name, attr, deps) in rels ]


def filter_dependencies(deps, attributes):
    return [
        Dep(left, right & attributes)
        for (left, right) in canonical_cover(deps, silent=True)
        if left <= attributes and (right & attributes)
    ]


def decompose(relation, deps):
    z = [('', Attr(relation), deps)]
    print('Beginne mit R := ({{{}}}, {})'.format(', '.join(sorted(relation)),
                                                 deps))
    in_bcnf = []
    while z:
        name, r, fd = z.pop(0)
        keys = find_keys(r, fd)
        for d in fd:
           if len(d.left.intersection(d.right)) == 0 and not is_super_key(d.left, r, deps):
                print(
                    'R_{} := ({{{}}}, {}) ist nicht in BCNF, aufgrund von {}, da {} kein Superschlüssel ist.'
                    .format(name, ', '.join(sorted(r)), fd, d, ''.join(sorted(d.left))))
                x1 = d.left | d.right
                fd1 = filter_dependencies(fd, x1) # [f for f in fd if (f.left | f.right) <= x1]
                name1 = name + '1'
                r1 = (name1, Attr(x1), fd1)
                print('Zerlege in:')
                print('- R_{} := ({{{}}}, {})'.format(name1, ', '.join(sorted(x1)), fd1))
                x2 = r - d.right
                fd2 = filter_dependencies(fd, x2) # [f for f in fd if (f.left | f.right) <= x2]
                name2 = name + '2'
                r2 = (name2, Attr(x2), fd2)
                print('- R_{} := ({{{}}}, {})'.format(name2, ', '.join(sorted(x2)), fd2))
                # Check if both dependencies add up to fd to see whether the decomposition is lossless.
                z.append(r1)
                z.append(r2)
                break
        else:

            new_deps = [Dep(d.left, r - d.left) for d in fd]
            in_bcnf.append((r, new_deps))
    return in_bcnf
