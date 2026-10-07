dotfiles
=========

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~sh
$ python -c "$(curl -s https://raw.githubusercontent.com/kui/ansi_pixels/master/tool/ansi-pixels.py)" "eAGrVirIrEjNCc6sSlWyMjLVUUpKd87PyS9SslIKcndy1DDQUYAiPQtNJR2lNLh0eEZmSSpQJBnEDy5ITAYaoGQBFMjLTy_KTClWsiopKk3VgZgP5EVHm-tghbE6KDIGeGQM8MgY4DQNKkeCPdhkDJDtR5IxIEPGHFUcwwXIMrG1ADOLYIc="
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<a href="https://kui.github.io/ansi_pixels/#eAGrVirIrEjNCc6sSlWyMjLVUUpKd87PyS9SslIKcndy1DDQUYAiPQtNJR2lNLh0eEZmSSpQJBnEDy5ITAYaoGQBFMjLTy_KTClWsiopKk3VgZgP5EVHm-tghbE6KDIGeGQM8MgY4DQNKkeCPdhkDJDtR5IxIEPGHFUcwwXIMrG1ADOLYIc="><img alt="kui" src="kui.png"></a>

kui's files such as dotfiles, scripts or device settings


Installation
--------------

In your terminal:

```
curl -s https://raw.githubusercontent.com/kui/dotfiles/master/init.sh | bash
```

### On a machine whose default GitHub account is not `kui`

When the default SSH key (`~/.ssh/id_ed25519`) belongs to another account (e.g. a work PC),
use a separate key for `kui` and clone this repository through a host alias:

1. Put the `kui` key at `~/.ssh/id_ed25519_personal` and add to `~/.ssh/config`:

   ```
   Host github-personal
       HostName github.com
       User git
       IdentityFile ~/.ssh/id_ed25519_personal
       IdentitiesOnly yes
   ```

2. Clone and install:

   ```
   git clone git@github-personal:kui/dotfiles.git ~/.dotfiles
   ~/.dotfiles/init.sh
   ```

3. Fill in `~/.gitconfig.local` following the comments in it
   (default `user.email`, `insteadOf` for `kui/`, `includeIf` for personal repositories).


Profile
--------

* site: http://k-ui.jp/
* twitter: [@k_ui](https://twitter.com/k_ui)
* tumblr:  [k-ui](http://k-ui.tumblr.com)
* github: [kui](https://github.com/kui)
* qiita: [k_ui](http://qiita.com/k_ui)
