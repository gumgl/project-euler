import argparse
import sys
import importlib.util
from pathlib import Path

def run_solution(year, day, use_example):
    day_str = day.zfill(2)
    base_dir = Path(__file__).parent
    year_dir = base_dir / f"Y{year}"
    day_dir = year_dir / f"D{day_str}"
    
    script_path = day_dir / "solution.py"
    input_filename = "example.txt" if use_example else "input.txt"
    input_path = day_dir / input_filename
    
    if not script_path.exists():
        return False, f"Script not found at {script_path}"
        
    if not input_path.exists():
        return False, f"Input file not found at {input_path}"
        
    # Read input
    with open(input_path, "r") as f:
        input_data = f.read()
    
    # Import the module dynamically
    module_name = f"aoc_{year}_{day_str}"
    spec = importlib.util.spec_from_file_location(module_name, script_path)
    if spec is None or spec.loader is None:
        return False, f"Could not load spec for {script_path}"
        
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    
    try:
        spec.loader.exec_module(module)
    except Exception as e:
        return False, f"Error executing module: {e}"
    
    # Execute solve
    if hasattr(module, "solve"):
        try:
            part1, part2 = module.solve(input_data)
            print(f"Part 1: {part1}")
            print(f"Part 2: {part2}")
            print()
            return True, None
        except Exception as e:
            import traceback
            traceback.print_exc()
            return False, f"Error running solve(): {e}"
    else:
        return False, f"The module {script_path} does not have a 'solve(input_data)' function."

def main():
    parser = argparse.ArgumentParser(description="Run Advent of Code solutions.")
    parser.add_argument("year_or_file", type=str, help="The year of the challenge (e.g., 2024) OR path to solution file")
    parser.add_argument("day", type=str, nargs="?", help="The day of the challenge (e.g., 10). If omitted, runs all days.")
    parser.add_argument("-e", "--example", action="store_true", help="Use the example input file")
    
    args = parser.parse_args()
    
    # Check if first argument is a file path
    arg_path = Path(args.year_or_file)
    if args.year_or_file.endswith(".py") and arg_path.exists():
        # Try to extract year and day from path
        # Expected structure: .../Y<year>/D<day>/solution.py
        
        parts = arg_path.parts
        year = None
        day = None
        
        for part in parts:
            if part.startswith("Y") and part[1:].isdigit() and len(part) == 5:
                year = part[1:]
            elif part.startswith("D") and part[1:].isdigit() and len(part) == 3:
                day = part[1:]
        
        if year and day:
            print(f"Detected Year: {year}, Day: {day} from path")
            success, error = run_solution(year, day, args.example)
            if not success:
                print(f"Error: {error}")
                sys.exit(1)
            return
        else:
             print(f"Could not extract Year and Day from path: {args.year_or_file}")
             sys.exit(1)

    # If not a file, treat as year
    year = args.year_or_file
    
    if args.day:
        success, error = run_solution(year, args.day, args.example)
        if not success:
            print(f"Error: {error}")
            sys.exit(1)
    else:
        # Run all days
        found_any = False
        for d in range(1, 26):
            day_str = str(d)
            day_padded = day_str.zfill(2)
            base_dir = Path(__file__).parent
            
            # Check for Dxx/solution.py
            year_dir = base_dir / f"Y{year}"
            day_dir = year_dir / f"D{day_padded}"
            script_path = day_dir / "solution.py"
            
            if script_path.exists():
                found_any = True
                success, error = run_solution(year, day_str, args.example)
                if not success:
                    print(f"Error running day {day_str}: {error}")
        
        if not found_any:
            print(f"No solutions found for year {year}")

if __name__ == "__main__":
    main()
