import os
import re

# Directories to skip
skip_dirs = {'./tools', './src/gtk', './src/gtk2', './_build'}

module_force_link_calls = []

for root, dirs, files in os.walk('.'):
    # Normalize path to always start with ./
    rel_root = os.path.relpath(root, '.')
    rel_root = './' + rel_root if rel_root != '.' else '.'

    # Skip specified directories
    if any(rel_root.startswith(skip_dir) for skip_dir in skip_dirs):
        continue

    ml_files = [f for f in files if f.endswith('.ml')]
    for ml_file in ml_files:
        module_name = os.path.splitext(ml_file)[0]
        ml_path = os.path.join(root, ml_file)
        
        print(f"Processing file: {ml_path}")
        
        modified = False  # Flag to track if .ml was modified
        
        try:
            with open(ml_path, 'r+', encoding='utf-8') as f:
                content = f.read()

                # Check if 'let () =' or 'let _ =' exists
                if re.search(r'\blet\s+\(\)\s*=', content) or re.search(r'\blet\s+_\s*=', content):
                    print(f"  Adding force_link to {ml_path}")
                    if not content.endswith('\n'):
                        content += '\n'
                    content += '\nlet force_link () = ()\n\n'
                    f.seek(0)
                    f.write(content)
                    f.truncate()
                    
                    modified = True  # Mark as modified
                    
                    # Prepare module force_link call
                    capitalized_module = module_name[0].upper() + module_name[1:]
                    module_force_link_calls.append(f'{capitalized_module}.force_link ()')
                else:
                    print(f"  Skipped (no 'let ()' or 'let _')")

        except UnicodeDecodeError as e:
            print(f"ERROR reading file (non-UTF8?): {ml_path}")
            print(f"Exception: {e}")
            continue  # Skip this file
        
        # --- Update .mli only if .ml was modified ---
        if modified:
            mli_file = module_name + '.mli'
            mli_path = os.path.join(root, mli_file)
            if os.path.exists(mli_path):
                try:
                    with open(mli_path, 'a', encoding='utf-8') as f:
                        f.write('\nval force_link: unit -> unit\n')
                except Exception as e:
                    print(f"ERROR writing to mli file: {mli_path}")
                    print(f"Exception: {e}")

# Output the list of force_link calls
print("\n=== Module force_link Calls ===")
for call in module_force_link_calls:
    print(call + ";")
