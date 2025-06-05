
# GlobalTechAnalytics

<!-- badges: start -->
<!-- badges: end -->

GlobalTechAnalytics is a package designed to analyze Resource data and provide insights into Resource Attrition and Project delivery. It includes a Shiny application for interactive data exploration.


### 1. Install the necessary tools (if not already installed)

If you don’t have the `pak` or `devtools` packages installed, you can install them first. `pak` is the recommended tool for installing dependencies from GitHub.

```r
# Install 'pak' (if not already installed)
install.packages("pak")

# Or install 'devtools' (alternative method)
install.packages("devtools")
```

### 2. Install **GlobalTechAnalytics** from GitHub

Once `pak` or `devtools` is installed, you can install **GlobalTechAnalytics** from GitHub:

```r
# Using pak (recommended)
pak::pak("Anna-KG/GlobalTechAnalytics")

# Or using devtools
devtools::install_github("Anna-KG/GlobalTechAnalytics")
```

This will install **GlobalTechAnalytics** along with all its dependencies, including **shiny**.

### 3. Install Shiny (if not already installed)

If **shiny** is not already installed on your system, install it by running:

```r
install.packages("shiny")
```

## Example

After installation, you can load the package and run the Shiny app. Here's a simple example of how to get started:

```r
# Load the package
library(GlobalTechAnalytics)

# Run the Shiny app
shiny::runApp(system.file("shiny", package = "GlobalTechAnalytics"))
```

### What Happens Next?
- This will launch the Shiny app in your default web browser.
- The app will be accessible at `http://127.0.0.1:xxxx` in your browser, where `xxxx` is a dynamic port number assigned to the app.

## Usage

Once the app is running, you can interact with it directly through the browser. 

## Contributing

I welcome contributions! To contribute to **GlobalTechAnalytics**, please follow these steps:

1. Fork the repository on GitHub.
2. Create a new branch for your feature (`git checkout -b feature/feature-name`).
3. Commit your changes (`git commit -m 'Add new feature'`).
4. Push to your branch (`git push origin feature/feature-name`).
5. Open a pull request to merge your changes into the main repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

For questions, suggestions, or issues, please contact the repository owner via the GitHub repository page.

