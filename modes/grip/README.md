# About

The grip-mode renders Markdown files and display them in the (built-in) web browser.

webpage: https://github.com/seagle0128/grip-mode

# Configure

`grip` uses the GitHub API to render the markdown file. There is an API call limit for anonymous users: `GitHub Rate Limit Reached`. To avoid the limit, you have to log in with a GitHub Account. You can create a [Personal Access Token](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) for the `grip-mode`. The token does **not** require any permissions. After you create the token, you must create the `.authinfo` file in the root of your home directory. Add the following line to the file:

```
machine api.github.com login <login e-mail adress> password <token>
```

The `grip-mode` automatically detects the file an sets up the necessary variables after starting Emacs.
