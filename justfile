p1 day:
  time scala-cli . -M day{{ day }}.part1

p2 day:
  time scala-cli . -M day{{ day }}.part2

run day:
  just p1 {{ day }}
  just p2 {{ day }}
