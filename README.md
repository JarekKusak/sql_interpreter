# Jednoduchý SQL interpret v jazyce Haskell

Umožňuje zakládání tabulek, vkládání záznamů, dotazování (queries), včetně klauzulí WHERE.

Níže je příklad sekvence příkazů pro založení tabulky a na její dotazování.

```bash
main
CREATE TABLE StudentsWithAge (ID,Name,Age)
INSERT INTO StudentsWithAge (1,Alice,22)
INSERT INTO StudentsWithAge (2,Bob,19)
INSERT INTO StudentsWithAge (3,Charlie,25)
SELECT (ID,Name) FROM StudentsWithAge WHERE Age > 20
SELECT (ID) FROM StudentsWithAge WHERE Name = 'Alice'
```
