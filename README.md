
# osprey

`osprey` is an R package for crowd sourcing development of analysis functions to
create TLGs. We also provide teal modules for outputs in `osprey` in the
`teal.osprey` package.

## Installation

### Clone and install manually

1. Clone the repository

   The repository can be downloaded directly from the `github.com` site as an archive (see [Github tutorial on cloning to learn more](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository-from-github/cloning-a-repository)).
   Alternatively, Git command line tools offer the same functionality, without the need for manual downloading and unpacking the archive, but require to authenticate to Github. You can authenticate using a key pair or a Personal Access Token (PAT).
   Please refer to excellent Github tutorials on [connecting to Github using SSH](https://docs.github.com/en/github/authenticating-to-github) or [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token).

   1. Using PAT. Input in the Git Bash console, PowerShell or any Linux shell:

      ```shell
      $ git clone https://github.com/insightsengineering/osprey.git
      Username: your_username_goes_here
      Password: your_token_goes_here
      ```

    1. Using SSH. If set up properly, the repository is ready to be cloned executing:

       ```shell
       git clone https://github.com/insightsengineering/osprey.git
       ```
   This creates a sub-directory `osprey` containing the cloned repository.

2. Build and install

   The native R tools provide a quick way to install a package. Run in PowerShell or any Linux shell:

   ```shell
   R CMD build osprey
   ```

   This command builds the package and creates an archive. The name of the archive is output by the command at then of building. Then input in the shell:

   ```shell
   Rscript -e 'install.packages("name_of_the_archive")
   ```

   Here is an example of a real command (with name_of_the_archive substituted by the output of the build command):

   ```shell
   Rscript -e 'install.packages("osprey_0.1.10.9000.tar.gz")'
   ```

## Development

Everyone is welcome to share their R functions in `osprey`.

### Requirements

To qualify to `osprey` package, TLG code has to:

1. Be a function that produces an output
2. Have a working example
3. Use [roxygen documentation](http://r-pkgs.had.co.nz/man.html) for creation of Help
4. Have a help page
5. Be compatible with pkgdown (should be when points 1-4 are OK)

### Instructions

For this to work you need to have access to libraries `pkgdown` and
`scda`.

That is, follow the following steps:

1. Clone the osprey repository
2. Create a branch
3. Develop a function
4. Create roxygen comments (ctrl+shift+alt+R or copy from earlier)
5. Document (ctrl+shift+D)
6. Install and Restart
7. Test your function with working example
8. Create webpage (in console: `pkgdown::build_site()`)
9. When everything works, make pull request.
