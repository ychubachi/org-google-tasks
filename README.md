[![Coverage Status](https://coveralls.io/repos/github/ychubachi/org-sync-gtasks/badge.svg)](https://coveralls.io/github/ychubachi/org-sync-gtasks)

# Org Sync GTasks
Synchronize Org TODO lists and Google Tasks.

This package provides two entriy points.

1. org-sync-gtasks-at-point
2. org-sync-gtasks-agenda

Move your cursor to an Org TODO headline and `M-x org-sync-gtasks-at-point`.
Then a new Google Tasks task is inserted to your default tasklist.  Some
properties of Google Tasks task will be added to the headline.  The GTASKS-ID
property of headlines is the most important property which enables this command
to determin the headlines are synchronized to Google Tasks items.

If you edit the task on Google Tasks, this command will pull the changes to your
Org TODO headline.  Or if you edit the Org TODO headline and use this command
again, Google Tasks task will be updated.

`M-x org-sync-gtasks-agenda` applies the `org-sync-gtasks-at-point` command to
all your TODO headlines with a GTASKS-ID property in Org agenda files.  Plus,
the tasks which are not in Org agend files are inserted as Org headlines to
current cursor position.

# Configration

Obtain your credential file with JSON format from Google Developres Console.
Don't forget to give Google Tasks access permitions to it.  Put your JSON file
to `~/.client_secret.json`.  You can change this path by customizing
`org-sync-gtasks--client-secret-json` variable.

# Limitations
- This command can handle only your default tasklist.
- First time you try to connect to Google Tasks API, an error might be occured
  in OAuth2 process.
