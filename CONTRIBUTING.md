# Contributing to The Blue Water Toolkit

Thank you for your interest in improving The Blue Water Toolkit! We welcome contributions from hydrologists, data scientists, agricultural researchers, and software engineers alike. 

To maintain code quality and protect the future sustainability of this framework, we ask all contributors to follow the guidelines below.

---

## 🛠️ How to Contribute

### 1. Reporting Bugs & Requesting Features
* Open an issue using the GitHub **Issues** tab.
* Provide a clear description of the bug or feature request.
* If reporting an issue with data processing (`01_Format_Input_Data.Rmd`) or the execution engine (`02_blue-water.Rmd`), please include a minimal reproducible example (reprex) and any relevant error logs.

### 2. Submitting Code Changes (Pull Requests)
* **Fork** the repository and create your branch from `main`.
* **Keep it modular:** If you are updating biophysical or visualization algorithms, keep the core engine logic separated inside the `code/functions/` directory.
* **Do not include raw data:** Avoid committing large, localized climate or demographic survey data into the `data/raw/` directory unless explicitly discussed.
* **Format your code:** Ensure your `.R` scripts and `.Rmd` files are clean and documented. Do not remove the hidden license comment blocks (`<!-- -->`) from the tops of `.Rmd` files.

---

## 📜 Contributor License Agreement (CLA)

By submitting a Pull Request (PR) to this repository, you agree to the following terms regarding your contribution. This agreement ensures that the core codebase remains open under the LGPL-3.0 while allowing the original creator to manage alternative licensing models in the future.

1. **License Grant:** You grant the project maintainer (**Rutuja-Chitra Tarak**) a non-exclusive, perpetual, irrevocable, worldwide, royalty-free license to use, modify, distribute, and sub-license your contributions.
2. **Copyright Ownership:** You retain the copyright to your individual contribution, but you grant the project maintainer the right to re-license the collective codebase (including your contribution) under alternative commercial or open-source terms without further restriction.
3. **Original Work:** You confirm that your contribution is your own original work and that you have the legal right to grant these permissions. If your work was done on behalf of an employer or university, you confirm you have their permission to submit it.

*By clicking "Create pull request" or "Merge pull request" on GitHub, you officially accept these terms.*

---

## 👥 Academic & CRediT Attribution

We value academic transparency and fair attribution. If your code contribution introduces a new methodology or mathematical formulation that changes the core framework logic, please update the **Authors & Contributions** section in the `README.md` using the standard **CRediT (Contributor Roles Taxonomy)** guidelines.
