import os
import shutil

outputs = os.getenv("QUARTO_PROJECT_OUTPUT_FILES").split("\n")

for output in outputs:
  if not any("index_files" in line for line in open(output)):
    shutil.rmtree(os.path.join(os.path.dirname(output), "index_files"), ignore_errors = True)
