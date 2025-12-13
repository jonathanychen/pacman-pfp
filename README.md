# Pac-Man Q-Learning

Parallel reinforcement learning implementation for Pac-Man in Haskell.

## Project Structure

```
pacman/
├── app/
│   └── Main.hs              # Entry point with CLI
├── src/
│   ├── Types.hs             # Data types and structures
│   ├── Game.hs              # Game logic and visualization
│   ├── QLearning.hs         # Sequential Q-learning
│   └── ParallelQLearning.hs # Parallel Q-learning
├── grids/
│   └── simple.txt           # Sample grid file
├── test/
│   └── Spec.hs              # Unit tests
├── package.yaml             # Build configuration
└── stack.yaml               # Stack configuration
```

## Building

```bash
stack setup
stack build
```

## Running

### Demo Mode (Random Actions with Visualization)

Watch Pac-Man move randomly with visual feedback:

```bash
stack exec pacman-exe -- --demo --visualize
```

Or using short flags:
```bash
stack exec pacman-exe -- --demo -v
```

### Training Mode

**Sequential (1 core):**
```bash
stack exec pacman-exe -- --train --cores 1 --episodes 1000
```

**Parallel (8 cores):**
```bash
stack exec pacman-exe -- --train --cores 8 --episodes 1000 +RTS -N8
```

**With visualization of final learned policy:**
```bash
stack exec pacman-exe -- --train -c 4 -e 500 -v +RTS -N4
```

### Test Mode

Test a trained policy with visualization:
```bash
stack exec pacman-exe -- --test --visualize
```

## Command Line Options

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--demo` | | Run demo with random actions | Train mode |
| `--train` | | Run training mode | Train mode |
| `--test` | | Run testing mode | Train mode |
| `--visualize` | `-v` | Enable visualization | Disabled |
| `--cores N` | `-c N` | Number of cores to use | 1 |
| `--episodes N` | `-e N` | Number of training episodes | 1000 |
| `--grid FILE` | `-g FILE` | Grid file to load | grids/simple.txt |

## Grid File Format

Grid files should be text files where:
- `1` = wall
- `0` = pellet (food for Pac-Man to collect)
- `G` = ghost starting position
- `P` = Pac-Man starting position

Example (`grids/simple.txt`):
```
1111111111
1000000001
11100000G1
1000000001
10011100G1
10G000G001
100P000001
1111111111
```

## Visualization

When visualization is enabled (`--visualize` or `-v`):

- The grid is displayed in the terminal
- `P` = Pac-Man
- `G` = Ghost
- `·` = Pellet
- `█` = Wall
- The score, pellets remaining, and game status are shown
- Each step is displayed with a 500ms delay

## Example Usage

1. **Quick demo to see the game:**
   ```bash
   stack exec pacman-exe -- --demo -v
   ```

2. **Train on 4 cores for 2000 episodes:**
   ```bash
   stack exec pacman-exe -- --train -c 4 -e 2000 +RTS -N4
   ```

3. **Train and visualize the learned policy:**
   ```bash
   stack exec pacman-exe -- --train -c 2 -e 1000 -v +RTS -N2
   ```

4. **Use a custom grid:**
   ```bash
   stack exec pacman-exe -- --demo -v -g grids/custom.txt
   ```

## Testing

```bash
stack test
```

## Performance Benchmarking

To benchmark parallel speedup, run with different core counts:

```bash
# 1 core
stack exec pacman-exe -- --train -c 1 -e 5000

# 2 cores
stack exec pacman-exe -- --train -c 2 -e 5000 +RTS -N2

# 4 cores
stack exec pacman-exe -- --train -c 4 -e 5000 +RTS -N4

# 8 cores
stack exec pacman-exe -- --train -c 8 -e 5000 +RTS -N8
```

Compare the "Training completed in X seconds" output to calculate speedup.

## Creating Custom Grids

1. Create a new text file in the `grids/` directory
2. Use the format: `1` (wall), `0` (pellet), `G` (ghost), `P` (Pac-Man)
3. Make sure the grid is surrounded by walls
4. Place exactly one `P` for Pac-Man
5. Place one or more `G` for ghosts

Example:
```
111111
1P00G1
100001
111111
```

## Troubleshooting

**Issue**: "Stack not found"
- Solution: Install Stack from https://docs.haskellstack.org/

**Issue**: Compilation errors
- Solution: Run `stack clean` then `stack build`

**Issue**: Visualization doesn't clear screen properly
- Solution: Make sure your terminal supports ANSI escape codes

**Issue**: Program runs slowly in visualization mode
- Solution: This is expected - visualization adds delays for viewing. Turn off `-v` for benchmarking.

## Notes for Development

- Turn OFF visualization (`-v` flag) when benchmarking parallel performance
- The visualization is meant for debugging and demonstrating correctness
- For large-scale training, use `--train` without `-v`
- The Q-table grows with the state space - larger grids = more memory

## Authors

- Jonathan Chen (jyc2183)
- Kevin Wang (kjw2169)

Columbia University - Fall 2025