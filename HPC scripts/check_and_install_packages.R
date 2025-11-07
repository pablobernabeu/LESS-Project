# Script to verify and install required R packages in the HPC container
# Run this script inside the Singularity container to ensure all dependencies are available

cat("Checking R version...\n")
print(R.version.string)
cat("\n")

# List of required packages for consolidated EEG analysis
# Note: allFit is a function in lme4, not a separate package
required_packages <- c(
    "dplyr",
    "readxl",
    "RcppEigen",
    "lme4",
    "lmerTest",
    "emmeans",
    "MuMIn"
)

cat("Checking required packages...\n\n")

# Check which packages are missing
missing_packages <- c()
for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        pkg_version <- packageVersion(pkg)
        cat(sprintf("✓ %s (version %s) - INSTALLED\n", pkg, pkg_version))
    } else {
        cat(sprintf("✗ %s - MISSING\n", pkg))
        missing_packages <- c(missing_packages, pkg)
    }
}

cat("\n")

# Report summary
if (length(missing_packages) == 0) {
    cat("SUCCESS: All required packages are installed!\n")
} else {
    cat("WARNING: The following packages are missing:\n")
    for (pkg in missing_packages) {
        cat(sprintf("  - %s\n", pkg))
    }
    cat("\n")

    # Attempt to install missing packages
    cat("Attempting to install missing packages...\n\n")

    for (pkg in missing_packages) {
        cat(sprintf("Installing %s...\n", pkg))
        tryCatch(
            {
                install.packages(pkg,
                    repos = "https://cloud.r-project.org/",
                    dependencies = TRUE, quiet = FALSE
                )
                cat(sprintf("✓ %s installed successfully\n\n", pkg))
            },
            error = function(e) {
                cat(sprintf("✗ Failed to install %s: %s\n\n", pkg, e$message))
            }
        )
    }
}

# Final verification
cat("\n=== FINAL PACKAGE STATUS ===\n")
for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        pkg_version <- packageVersion(pkg)
        cat(sprintf("✓ %s (version %s)\n", pkg, pkg_version))
    } else {
        cat(sprintf("✗ %s - STILL MISSING\n", pkg))
    }
}

# Check that allFit function is available in lme4
cat("\n=== CHECKING FUNCTIONS ===\n")
if (requireNamespace("lme4", quietly = TRUE)) {
    library(lme4)
    if (exists("allFit", where = "package:lme4", mode = "function")) {
        cat("✓ allFit function (from lme4)\n")
    } else {
        cat("✗ allFit function NOT FOUND in lme4\n")
    }
} else {
    cat("✗ Cannot check allFit - lme4 not available\n")
}

cat("\nPackage check complete!\n")
