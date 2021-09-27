from collections import namedtuple
import re
from string import ascii_uppercase
import itertools

Dep = namedtuple('Dep', ['left', 'right'])


def dep_to_str(d):
    return '{}→{}'.format(''.join(sorted(d.left)) or '∅',
                          ''.join(sorted(d.right)) or '∅')


Dep.__repr__ = dep_to_str


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
            return values
    return values


def is_super_key(candidate, attributes, deps):
    return set(attributes) <= closure(candidate, deps)


def is_key(candidate, attributes, deps):
    return is_super_key(candidate, attributes, deps) and not any(
        is_super_key(candidate - {a}, attributes, deps) for a in candidate)


def find_keys(relation, deps):
    def rec(attributes):
        if is_super_key(attributes, relation, deps):
            return frozenset.union(
                *[rec(attributes - frozenset({x}))
                  for x in attributes]) or frozenset({attributes})
        else:
            return frozenset()

    return [frozenset(key) for key in rec(frozenset(relation))]


def canonical_cover(deps):
    remaining = deps.copy()
    left_reduced = []
    print('Linksreduktion:')
    while remaining:
        d = remaining[0]
        # print('Betrachte {}'.format(d))
        left, right = d
        for a in sorted(left):
            # print('Hülle ohne {}: {}'.format(a, closure(left - {a}, remaining + final)))
            if right <= closure(left - {a}, remaining + left_reduced):
                new_dep = Dep(left - {a}, right)
                print('- {} ist reduzierbar um {} zu {}.'.format(
                    d, a, new_dep))
                remaining.insert(1, new_dep)
                break
        else:
            # Non-reducible
            print('- {} ist nicht reduzierbar.'.format(d))
            left_reduced.append(d)
        remaining.pop(0)

    remaining = left_reduced
    right_reduced = []
    print('Rechtsreduktion:')
    while remaining:
        d = remaining.pop(0)
        left, right = d
        for b in sorted(right):
            if b in closure(
                    left,
                    remaining + right_reduced + [Dep(left, right - {b})]):
                new_dep = Dep(left, right - {b})
                print('- {} ist reduzierbar um {} zu {}.'.format(
                    d, b, new_dep))
                remaining.insert(0, new_dep)
                break
        else:
            print('- {} ist nicht reduzierbar.'.format(d))
            right_reduced.append(d)

    print('Entferne leere Abhaengigkeiten:')
    remaining = []
    for dep in right_reduced:
        if dep.right != set():
            remaining.append(dep)
        else:
            print('- {} geloescht.'.format(dep))
    final = []
    print('Zusammenfassung der Abhaengigkeiten:')
    while remaining:
        left, right = remaining.pop(0)
        new_right = right
        output = '- {}, '.format(Dep(left, right))
        for dep in remaining:
            if dep.left == left:
                new_right = new_right | dep.right
                output += '{}, '.format(dep)
                remaining.remove(dep)
        final.append(Dep(left, new_right))
        if right != new_right:
            output = output[:-2] + ' wird zusammengefasst zu {}'.format(Dep(left, new_right))
            print(output)
    return final


Rel = namedtuple('Rel', ['name', 'attrs', 'deps'])

# def to2NF(relation, deps):
#     key_candidates = find_keys(relation, deps)
#     npas = set(relation) - frozenset.union(*key_candidates)
#     # Find all true subsets of candidate keys
#     print(key_candidates)
#     part_keys = frozenset.union(*(frozenset(powerset(key)) for key in key_candidates)) - frozenset(key_candidates)
#     return part_keys


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
    return rels


def decompose(relation, deps):
    z = [('', frozenset(relation), deps)]
    print('Beginne mit R := ({{{}}}, {})'.format(', '.join(sorted(relation)),
                                                 deps))
    in_bcnf = []
    while z:
        name, r, fd = z.pop(0)
        keys = find_keys(r, fd)
        for d in fd:
            if len(d.left.intersection(
                    d.right)) == 0 and not is_super_key(d.left, r, deps):
                print(
                    'R_{} := ({{{}}}, {}) ist nicht in BCNF, aufgrund von {}, da {} kein Superschlüssel ist.'
                    .format(name, ', '.join(sorted(r)), fd, d, ''.join(sorted(d.left))))
                x1 = d.left | d.right
                fd1 = [f for f in fd if (f.left | f.right) <= x1]
                name1 = name + '1'
                r1 = (name1, x1, fd1)
                print('Zerlege in:')
                print('- R_{} := ({{{}}}, {})'.format(name1, ', '.join(sorted(x1)), fd1))
                x2 = r - d.right
                fd2 = [f for f in fd if (f.left | f.right) <= x2]
                name2 = name + '2'
                r2 = (name2, x2, fd2)
                print('- R_{} := ({{{}}}, {})'.format(name2, ', '.join(sorted(x2)), fd2))
                # Check if both dependencies add up to fd to see whether the decomposition is lossless.
                z.append(r1)
                z.append(r2)
                break
        else:

            new_deps = [Dep(d.left, r - d.left) for d in fd]
            in_bcnf.append((r, new_deps))
    return in_bcnf
