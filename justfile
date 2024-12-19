p1 day:
  @time scala-cli . -M day{{ day }}.part1 -deprecation

p2 day:
  @time scala-cli . -M day{{ day }}.part2 -deprecation

run day:
  @echo "🎄 Running Day {{day}} 🎄\n"
  @echo "Part 1:"
  @just p1 {{day}}
  @echo "\nPart 2:"
  @just p2 {{day}}
