# -*- coding: utf-8 -*-
import sys
from grammalecte import grammar_checker

def main():
    sys.stdin.reconfigure(encoding='utf-8')
    sys.stdout.reconfigure(encoding='utf-8')

    gc = grammar_checker.GrammarChecker("fr")
    text = sys.stdin.read()
    # On récupère les erreurs sous forme détaillée
    _, errors = gc.getParagraphWithErrors(text)
    lines = text.split('\n')

    for err in errors:
        start = err['nStart']
        end = err['nEnd']
        msg = err['sMessage'].replace('\n', ' ')
        # Calculer la ligne et la colonne approximatives
        char_count = 0
        line_num = 1
        col_num = 1
        for line in lines:
            if char_count + len(line) + 1 > start:
                col_num = start - char_count + 1
                break
            char_count += len(line) + 1
            line_num += 1

        # Affiche au format flycheck
        print(f"<stdin>:{line_num}:{col_num}: warning: {msg}")

if __name__ == "__main__":
    main()
