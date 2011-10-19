
# backups-mode for emacs

## Inspiration
The inspiration for this came after reading [John Siracusa's review of the Document Model](http://arstechnica.com/apple/reviews/2011/07/mac-os-x-10-7.ars/7#document-model) new to Mac OS X Lion. This new framework on the Mac allows for the following:

* All documents are automatically saved. This includes when the user closes the document or quits the application.
* Old versions of the document are automatically stored and accessible.
* The user has the ability to manually save a verion of the document.
* Old versions can be viewed, diffed, and reverted.
* Reverting an old version saves the current file as a version and copies the selected version to become the current file.

So I set out to emulate these features in emacs. emacs already has its own rudimentary version control system that can be turned on simply by configuring emacs. It can also be configured to keep those backup files tucked away in a central directory. It can also be configured to automatically save (without prompting) your files when you kill the buffer or quit emacs. In the event that emacs crashes, you can also restore your file from an autosave file emacs creates for you. So it has all of that out of the box.

What it doesn't have (or at least I couldn't find) is the ability to easily find, view, diff, and revert those versioned backup files. That is where *backups-mode.el* comes into play. While editing any file-based buffer in emacs, you can now do two extra things. You can list all backups and you can explicily save a version.

## Disclaimer
Using the emacs version control functionality should not be a replacement for a proper version control system such as cvs, svn, git, mercurial, and the like. So if you are developing a project or typing a manuscript, you'll want to use one of those systems. I find this useful for the less important stuff such as blog posts or exploritory programming.

## Installation
    git clone git@github.com:chadbraunduin/backups-mode.git
    cd backups-mode
    # copy the necessary files to your emacs load-path
    cp backups-mode.el bm-utilities.el backup-walker.el ~/.emacs.d/
    # this assumes ~/.emacs.d/ is in your emacs load-path
    # add the following to .emacs
    (require 'backups-mode)
    (backups-mode-start)

## Additional configuration
    ;; putting this in your .emacs will allow you to change version control settings. The following are the default settings found in backups-mode.el.
    (setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
[emacs documentation](http://www.gnu.org/software/emacs/elisp/html_node/Numbered-Backups.html) will explain these options.

If you are using Windows, you'll want to set these objects to be Windows specific:

* last-modified-date-command-function
* unknown-last-modified-date

By default, backups are saved to "~/.emacs.d/backups" and tramp file backups are saved to "~/.emacs.d/tramp-backups". These defaults can be changed by setting:

* emacs-directory
* backup-directory
* tramp-backup-directory

### My .emacs
As an example, here's the configuration from my .emacs file.

    (require 'backups-mode)
    (defvar backup-directory "~/.emacs-backups/backups/")
    (defvar tramp-backup-directory "~/.emacs-backups/tramp-backups/")
    (backups-mode-start)
    ;; keep all versions forever
    (setq delete-old-versions 1)

## Commands
While editing any file-based emacs buffer, there are some additional commands:

* save-version
 * This will version the previous saved copy of the file.
 * By default, this command can be done with control-c v ("\C-cv")

* list-backups
 * This will open a new buffer in backups-mode which will list all backups of the file.
 * By default, this command can be done with control-c b ("\C-cb")

* backup-walker
 * This will open a new buffer in backup-walker mode which will let you sequentially move through backup diffs.
 * By default, this command can be done with control-c w ("\C-cw")
 
* kill-buffer-prompt
 * This will allow the user to close a buffer without saving any changes
 * By default, this command can be done with control-c k ("\C-ck")
 
While in the backups-mode buffer, these are the commands:

* View Backup
 * This is done by aligning the cursor to a file's line, and hitting \[enter\]. 
 * Backup files will be opened read-only.

* Revert Backup
 * This is done by aligning the cursor to a file's line, and typing "R".
 * Reverting will save the current file as a version, then replace the current file with the chosen backup.

* Diff 2 Files
 * This is done by aligning the cursor to a file's line, and typing "d". This will mark that line as first file to diff. 
 * Then, you align the cursor to another file's line and type "d". This will run the "diff-function" command on the two selected files.
 
* Purge (delete) Backups
 * This is similar to Dired mode's batch delete, except Backups Mode uses "p" instead of "d" to mark backups for deletion. The choice of letters is because "d" is already being used to diff backups.
 * After marking one-to-many backups, "x" deletes all the marked backups.
 
## Backup Walker
I've adapted lewang's [backup-walker](https://github.com/lewang/backup-walker) project. Backup Walker gives you a different view on your backups. Whereas Backups Mode lists all of your backups, Backup Walker starts with a diff of the current file and the previous backup. You can then sequentially move through diffs of consecutive backup files. Often, if you know what you are looking for, Backup Walker can be more efficient than Backups Mode.

## Cleanup
The problem with creating N backup files per file is that over time you'll have generated a lot of backup files. Some of these backup files may even be orphaned if the original file is moved or deleted. I've taken two approaches for this problem:

### For local files
For local files, I've created the script "show-orphaned.sh" (found in the scripts directory). It goes through my backups directory and displays all orphaned backups. I've created a @daily crontab job to remove all of the orphaned backups.

    @daily /home/chadbraunduin/.emacs-backups/backups/show-orphaned.sh | xargs -r /bin/rm -f
    
### For tramp files
For tramp files, we cannot assume to be able to access the original file. Therefore, I've taken a more crude approach with tramp backups. I've scheduled a @daily crontab job that removes any tramp backups that have not be accessed in the past 180 days (roughly 6 months).

    @daily find /home/chadbraunduin/.emacs-backups/tramp-backups/ -type f -name "*.*~" -atime +180 | xargs -r /bin/rm -f

## rsnapshot configuration
I use rsnapshot for rsync backups to an external drive. I've decided I do not care to backup these emacs generated backup files. Therefore, I've added these two lines to /etc/rsnapshot.conf:

    exclude		/home/chadbraunduin/.emacs-backups/backups/*.*~
    exclude		/home/chadbraunduin/.emacs-backups/tramp-backups/*.*~

## Bugs and TODOs
As with most projects, this is still a work in progress. The known issues are:

* I am planning on adapting this project to work with git-mode work-in-progress backups (WIP).
* A [bug](https://github.com/chadbraunduin/backups-mode/issues/1) using this on Mac Os X has been reported. This is related to use the function copy-file. 
* I have not tested it in Windows, yet. I believe configuration changes will be necessary to make it work in that environment. Since I only use emacs in Linux, this is not a personal priority for me.
