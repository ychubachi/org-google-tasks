[![Coverage Status](https://coveralls.io/repos/github/ychubachi/org-sync-gtasks/badge.svg)](https://coveralls.io/github/ychubachi/org-sync-gtasks)

# Org Sync GTasks
Synchronize Org TODO lists and Google Tasks.

This package provides two entriy points.

1. org-sync-gtasks-at-point
2. org-sync-gtasks-agenda

Move your cursor to an Org TODO headline and `M-x org-sync-gtasks-at-point`.
Then a new task is inserted to your default Google Tasks tasklist.  Several
properties of a Google Tasks task will be added to the headline.  The GTASKS-ID
property of a headline is the most important property which enables this command
to determin the headline is synchronized to the Google Tasks item.

If you edit the task on Google Tasks side, using this command again will pull
the changes to your Org TODO headline.  Or if you edit the Org TODO headline and
use this command, the Google Tasks task will be updated.

`M-x org-sync-gtasks-agenda` applies the `org-sync-gtasks-at-point` command to
all your TODO headlines with a GTASKS-ID property in Org agenda files.  Plus,
the tasks which are not in Org agend files are inserted as Org headlines to
current cursor position.

# Configration

Obtain your credential file in JSON format from Google Developres Console.
Don't forget to give Google Tasks access permitions to it.  Put your JSON file
to `~/.client_secret.json`.  You can change this path by customizing
`org-sync-gtasks--client-secret-json` variable.

# Installation

If you use leaf with straight,

```
  (leaf org-sync-gtasks
    :straight (org-sync-gtasks :type git :host github
                          :repo "ychubachi/org-sync-gtasks"))
```

# Limitations
- This command can handle only your default tasklist of Google Tasks.
- First time you connects to Google Tasks API, an error might be occured
  in OAuth2 process.  Please try again in such case.
