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

    return [set(key) for key in rec(frozenset(relation))]


def canonical_cover(deps):
    remaining = deps.copy()
    left_reduced = []
    while remaining:
        d = remaining[0]
        # print('Betrachte {}'.format(d))
        left, right = d
        for a in sorted(left):
            # print('Hülle ohne {}: {}'.format(a, closure(left - {a}, remaining + final)))
            if right <= closure(left - {a}, remaining + left_reduced):
                new_dep = Dep(left - {a}, right)
                print('{} ist linksreduzierbar um {} zu {}.'.format(
                    d, a, new_dep))
                remaining.insert(1, new_dep)
                break
        else:
            # Non-reducible
            print('{} ist nicht linksreduzierbar.'.format(d))
            left_reduced.append(d)
        remaining.pop(0)

    remaining = left_reduced
    right_reduced = []
    while remaining:
        d = remaining.pop(0)
        left, right = d
        for b in sorted(right):
            if b in closure(left,
                            remaining + right_reduced + [Dep(left, right - {b})]):
                new_dep = Dep(left, right - {b})
                print('{} ist rechtsreduzierbar um {} zu {}.'.format(
                    d, b, new_dep))
                remaining.insert(0, new_dep)
                break
        else:
            print('{} ist nicht rechtsreduzierbar.'.format(d))
            right_reduced.append(d)

    remaining = [dep for dep in right_reduced if dep.right != set()]
    final = []
    while remaining:
        left, right = remaining.pop(0)
        new_right = right
        for dep in remaining:
            if dep.left == left:
                new_right = new_right | dep.right
                remaining.remove(dep)
        final.append(Dep(left, new_right))
    return final
