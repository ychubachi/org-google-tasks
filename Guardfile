# A sample Guardfile
# More info at https://github.com/guard/guard#readme

## Uncomment and set this to only include directories you want to watch
# directories %w(app lib config test spec features) \
#  .select{|d| Dir.exist?(d) ? d : UI.warning("Directory #{d} does not exist")}

## Note: if you are using the `directories` clause above and you are not
## watching the project directory ('.'), then you will want to move
## the Guardfile to a watched dir and symlink it back, e.g.
#
#  $ mkdir config
#  $ mv Guardfile config/
#  $ ln -s config/Guardfile .
#
# and, you'll have to watch "config/Guardfile" instead of "Guardfile"

guard :shell do
  watch(%r{^org-sync-gtasks\.el|test/org-sync-gtasks-test\.el$}) do |m|
    `UNDERCOVER_CONFIG='(setq undercover-force-coverage t) (undercover "org-sync-gtasks.el" (:report-format \'simplecov) (:send-report nil))' cask exec ert-runner`
  end
end
