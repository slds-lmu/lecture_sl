import os
import re
import json
import logging
import argparse

# Set up logging to both file and console
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S',
    filename='main.log',
    filemode='a'
)

console = logging.StreamHandler()
console.setLevel(logging.DEBUG)
console.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s', datefmt='%Y-%m-%d %H:%M:%S'))
logging.getLogger().addHandler(console)

logging.debug("-" * 20)

INSERT_PATTERN = re.compile(r"INSERT_(\w+)")
# Todo: awful code duplication, I need to refactor this later

def get_text_json_from_notebook(nb_path: str) -> dict[str, str]:
    """Return {label: text, …} for every cell whose first non-blank line starts with 'label:'."""
    LABEL_RE = re.compile(r'^\s*label:\s*(\S+)', re.IGNORECASE)

    with open(nb_path, 'r', encoding='utf-8') as f:
        nb = json.load(f)

    out = {}
    for cell in nb.get("cells", []):
        lines = cell.get("source", [])
        if not lines:
            continue

        # first non-empty line
        for idx, line in enumerate(lines):
            if line.strip():
                break
        else:
            continue  # entirely blank

        m = LABEL_RE.match(line)
        if not m:
            continue

        label = m.group(1).strip()
        body = "".join(lines[idx + 1 :]).strip()
        out[label] = body

    return out

def process_file(path, texts, main_folder, save_folder="inserted"):
    """
    Process .ipynb or .qmd files, replacing INSERT_<key> placeholders.
    Saves results in a subfolder under main_folder.
    """
    save_dir = os.path.join(main_folder, save_folder)
    os.makedirs(save_dir, exist_ok=True)

    file_name = os.path.basename(path)
    logging.debug(f"Processing file: {file_name}")

    _, ext = os.path.splitext(path)
    updated_keys = []
    modified = False

    if ext == '.ipynb':
        # JSON notebook
        with open(path, 'r', encoding='utf-8') as f:
            nb = json.load(f)

        for cell in nb.get('cells', []):
            if cell.get('cell_type') == 'markdown':
                source = cell.get('source', [])
                if isinstance(source, str):
                    lines = source.splitlines(keepends=True)
                else:
                    lines = source

                new_lines = []
                for line in lines:
                    def replace_match(match):
                        key = match.group(1)
                        if key in texts:
                            updated_keys.append(key)
                            return texts[key]
                        else:
                            error_msg = f"Key '{key}' not found in texts.json | {path}"
                            logging.error(error_msg)
                            
                            raise KeyError(error_msg)

                    new_line = INSERT_PATTERN.sub(replace_match, line)
                    if new_line != line:
                        modified = True
                    new_lines.append(new_line)

                cell['source'] = new_lines

        if modified:
            save_path = os.path.join(save_dir, os.path.basename(path))
            with open(save_path, 'w', encoding='utf-8') as f:
                json.dump(nb, f, indent=1, ensure_ascii=False)
            logging.info(f"Updated notebook: {save_path}")
            logging.debug(f"Keys updated: {updated_keys}")
            all_keys_updated = set(updated_keys) == set(texts.keys())
            if not all_keys_updated:
                in_json_not_in_file = set(texts.keys()) - set(updated_keys)
                in_file_not_in_json = set(updated_keys) - set(texts.keys())
                
                all_missing_keys = in_json_not_in_file | in_file_not_in_json

                okay_to_miss = ["colab_R_link", "colab_python_link"]
                if (len(all_missing_keys) > 1) or \
                    list(all_missing_keys)[0] not in okay_to_miss:
                        logging.warning(f"Not all keys updated. Keys in json but not in file: {in_json_not_in_file} | in file but not in json: {in_file_not_in_json}")
        else:
            logging.info(f"No placeholders in: {path}")

    elif ext == '.qmd':
        # Quarto markdown
        with open(path, 'r', encoding='utf-8') as f:
            content = f.read()

        def replace_match(match):
            key = match.group(1)
            if key in texts:
                updated_keys.append(key)
                return texts[key]
            else:
                error_msg = f"Key '{key}' not found in texts.json | {path}"
                logging.error(error_msg)
                raise KeyError(error_msg)

        new_content = INSERT_PATTERN.sub(replace_match, content)
        if new_content != content:
            modified = True

        if modified:
            save_path = os.path.join(save_dir, os.path.basename(path))
            with open(save_path, 'w', encoding='utf-8') as f:
                f.write(new_content)
            logging.info(f"Updated qmd: {save_path}")
            logging.debug(f"Keys updated: {updated_keys}")
            all_keys_updated = set(updated_keys) == set(texts.keys())
            if not all_keys_updated:
                in_json_not_in_file = set(texts.keys()) - set(updated_keys)
                in_file_not_in_json = set(updated_keys) - set(texts.keys())
                
                all_missing_keys = in_json_not_in_file | in_file_not_in_json

                okay_to_miss = ["colab_R_link", "colab_python_link"]
                if (len(all_missing_keys) > 1) or \
                    list(all_missing_keys)[0] not in okay_to_miss:
                        logging.warning(f"Not all keys updated. Keys in json but not in file: {in_json_not_in_file} | in file but not in json: {in_file_not_in_json}")
  
        else:
            logging.info(f"No placeholders in: {path}")
    else:
        logging.debug(f"Skipping unsupported file type: {path}")


def find_files(path):
    """
    Yield paths to all .ipynb and .qmd files in the given directory (non-recursive).
    If path is a file, yield if extension matches.
    """
    if os.path.isfile(path) and path.endswith(('.ipynb', '.qmd')):
        yield path
    elif os.path.isdir(path):
        for fname in os.listdir(path):
            if fname.endswith(('.ipynb', '.qmd')) and ('texts' not in fname):
                yield os.path.join(path, fname)


def main():
    parser = argparse.ArgumentParser(
        description='Replace INSERT_<key> placeholders in .ipynb and .qmd files using texts.json'
    )
    parser.add_argument(
        'folder', help='Folder containing .ipynb/.qmd files and the texts.json'
    )
    parser.add_argument(
        '--texts', "-t", help="path to the ipynb notebook with texts",
        default='texts.ipynb',
    )
    args = parser.parse_args()

    main_folder = os.path.abspath(args.folder)
    if not os.path.isdir(main_folder):
        logging.error(f"Not a directory: {main_folder}")
        return

    texts_path = os.path.join(main_folder, args.texts)
    if not os.path.isfile(texts_path):
        logging.error(f"texts not found: {texts_path}")
        logging.debug(f"ipynb files available: {[i for i in os.listdir(main_folder) if i.endswith('.ipynb')]}")
        return

    logging.info(f"Starting processing in: `{args.folder}` | Path: {main_folder}")
    texts = get_text_json_from_notebook(texts_path)

    files = list(find_files(main_folder))
    logging.debug(f"Found {len(files)} files to process: {[os.path.basename(file) for file in files]}")
    for file_path in files:
        process_file(file_path, texts, main_folder)


if __name__ == '__main__':
    main()
