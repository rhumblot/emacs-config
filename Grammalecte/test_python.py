from grammalecte import grammar_checker
gc = grammar_checker.GrammarChecker("fr")

text = "Je suis aller à la plage."
annotated, errors = gc.getParagraphWithErrors(text)
print(errors)
