;;; time-zones.el --- time zone lookups  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/time-zones

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; M-x time-zones gives you the ability to check the time for
;; any city in the world.
;;
;; This package would not be possible without these data sources:
;;
;; - Countries, states, and cities database:
;;   https://github.com/dr5hn/countries-states-cities-database
;;
;; - POSIX TZ database for Windows timezone support:
;;   https://github.com/nayarsystems/posix_tz_db
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

(require 'json)
(require 'map)
(require 'seq)
(require 'url)

;;; Code:

;; Declare functions which are defined elsewhere.
;; org-read-date is only needed for time-zones-jump-to-date,
;; which has the require for org within it to avoid a slow load
(declare-function org-read-date "org"
                  (&optional with-time to-time from-string prompt
                             default-time default-input inactive))

(defcustom time-zones-waking-hours '(6 . 22)
  "Cons cell defining waking hours as (START . END).
START is the hour when waking hours begin (default 6 for 6:00 AM).
END is the hour when waking hours end (default 22 for 10:00 PM).
Hours outside this range are considered sleeping hours and will
display a ☽ symbol."
  :type '(cons (integer :tag "Start hour (0-23)")
               (integer :tag "End hour (0-23)"))
  :group 'time-zones)

(defcustom time-zones-show-details nil
  "When non-nil, display UTC offset and DST information for each city."
  :type 'boolean
  :group 'time-zones)

(defcustom time-zones-show-help t
  "When non-nil, display header and bottom help text."
  :type 'boolean
  :group 'time-zones)

(defcustom time-zones-custom-timezones
  '(((timezone . "UTC")
     (flag . "🕒")
     (latitude . "0")
     (longitude . "0")))
  "List of additional time zones to pick from.

Each item is an alist containing keys like:

 `((timezone . \"UTC\")
   (country . ...)
   (state . ...)
   (city . ...)
   (flag . \"🕒\")
   (latitude . \"0\")
   (longitude . \"0\"))."
  :group 'time-zones
  :type '(repeat
          (alist :key-type (choice (const country)
                                   (const state)
                                   (const city)
                                   (const timezone)
                                   (const latitude)
                                   (const longitude)
                                   (const flag))
                 :value-type string)))

(defvar time-zones--timezones-cache nil
  "Cache for downloaded timezone data.")

(defvar time-zones--city-list nil
  "List of selected cities for display.")

(defvar time-zones--city-list-file
  (expand-file-name ".time-zones.el" user-emacs-directory)
  "File path for persisting the city list across sessions.")

(defvar time-zones--refresh-timer nil
  "Timer for auto-refreshing the display.")

(defvar time-zones--time-offset 0
  "Manual time offset in seconds.  When non-zero, timer is stopped.")

(defconst time-zones--version "0.3.2"
  "Version of the `time-zones' package.")

;;;###autoload
(defun time-zones ()
  "Open or switch to the time zones buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*time zones*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'time-zones-mode)
        (time-zones-mode)))
    (pop-to-buffer buffer)
    (when-let ((window (get-buffer-window "*time zones*" t))
               (height (count-lines (point-min) (point-max))))
      (fit-window-to-buffer window height height))))

;;;###autoload
(defun time-zones-version ()
  "Display the version of the `time-zones' package in the minibuffer."
  (interactive)
  (message "time-zones version %s" time-zones--version))

(defun time-zones-select-timezone ()
  "Download timezone data and allow user to select a timezone.
Uses `completing-read' for selection."
  (message "Fetching...")
  ;; Force in case M-x needs to close
  ;; before blocking on download.
  (redisplay)
  (let* ((data (or time-zones--timezones-cache
                   (setq time-zones--timezones-cache (time-zones--fetch-timezones))))
         (timezones (time-zones--extract-timezones data))
         (display-list (sort (map-keys timezones) #'string<))
         (selected (completing-read "Select timezone: " display-list nil t)))
    (map-elt timezones selected)))

(defun time-zones-add-city ()
  "Add a city to the timezone display."
  (interactive)
  (let ((city-data (time-zones-select-timezone)))
    (when city-data
      (push city-data time-zones--city-list)
      (time-zones--save-city-list)
      (time-zones--refresh-display))))

(defun time-zones-delete-city-at-point ()
  "Remove city at point from the display."
  (interactive)
  (let ((city-data (get-text-property (point) 'time-zones-timezone)))
    (when (and city-data
               (y-or-n-p (format "Delete %s? "
                                 (or (map-elt city-data 'city)
                                     (map-elt city-data 'state)
                                     (map-elt city-data 'timezone)))))
      (setq time-zones--city-list
            (seq-remove (lambda (city) (equal city city-data)) time-zones--city-list))
      (time-zones--save-city-list)
      (time-zones--refresh-display))))

(defun time-zones-refresh ()
  "Refresh the timezone display and restart live update."
  (interactive)
  (time-zones--start-timer)
  (time-zones--refresh-display))

(defun time-zones-time-forward (arg)
  "Move time forward by 15 minutes per ARG (default 1) and stop auto-refresh."
  (interactive "p")
  (time-zones--stop-timer)
  (setq time-zones--time-offset (+ time-zones--time-offset (* 15 60 arg)))
  (time-zones--refresh-display))

(defun time-zones-time-backward (arg)
  "Move time backward by 15 minutes per ARG (default 1) and stop auto-refresh."
  (interactive "p")
  (time-zones--stop-timer)
  (setq time-zones--time-offset (- time-zones--time-offset (* 15 60 arg)))
  (time-zones--refresh-display))

(defun time-zones-time-forward-hour (arg)
  "Move time forward by 1 hour per ARG (default 1) and stop auto-refresh."
  (interactive "p")
  (time-zones--stop-timer)
  (setq time-zones--time-offset (+ time-zones--time-offset (* 60 60 arg)))
  (time-zones--refresh-display))

(defun time-zones-time-backward-hour (arg)
  "Move time backward by 1 hour per ARG (default 1) and stop auto-refresh."
  (interactive "p")
  (time-zones--stop-timer)
  (setq time-zones--time-offset (- time-zones--time-offset (* 60 60 arg)))
  (time-zones--refresh-display))

(defun time-zones-jump-to-date ()
  "Jump to date and stop auto-refresh."
  (interactive)
  (require 'org)
  (time-zones--stop-timer)
  (setq time-zones--time-offset
        (fround (float-time (time-subtract (org-read-date t t) (current-time)))))
  (time-zones--refresh-display))

(defun time-zones-toggle-showing-details ()
  "Toggle display of UTC offset and DST information."
  (interactive)
  (setq time-zones-show-details (not time-zones-show-details))
  (time-zones--refresh-display)
  (message "Details %s" (if time-zones-show-details "shown" "hidden")))

(defun time-zones-toggle-showing-help ()
  "Toggle display of header and bottom help text."
  (interactive)
  (setq time-zones-show-help (not time-zones-show-help))
  ;; (time-zones-mode)
  (time-zones--refresh-display)
  (message "Help %s" (if time-zones-show-help "shown" "hidden")))

(defvar time-zones--timezones-url
  "https://raw.githubusercontent.com/dr5hn/countries-states-cities-database/refs/heads/master/json/countries%2Bstates%2Bcities.json.gz"
  "URL for countries, states, and cities database.")

(defvar time-zones--timezones-cache nil
  "Cache for downloaded timezone data.")

(defvar time-zones--posix-tz-url
  "https://raw.githubusercontent.com/nayarsystems/posix_tz_db/master/zones.json"
  "URL for IANA to POSIX TZ mappings database.")

(defvar time-zones--posix-tz-cache nil
  "Cache for downloaded POSIX TZ mapping data.")

(defconst time-zones--fallback-flag "🏴")

(defun time-zones--fetch-posix-tz-mappings ()
  "Download and parse the IANA to POSIX TZ mappings JSON data.
Returns an alist of (IANA-TZ . POSIX-TZ) pairs."
  (let ((url-mime-charset-string "utf-8")
        (url-automatic-caching nil)
        (data-buffer (url-retrieve-synchronously time-zones--posix-tz-url)))
    (with-current-buffer data-buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (let ((json-object-type 'alist))
        (json-read)))))

(defun time-zones--fetch-timezones ()
  "Download and parse the countries+states+cities JSON data."
  (let ((url-mime-charset-string "utf-8")
        (url-automatic-caching nil)
        (data-buffer (url-retrieve-synchronously time-zones--timezones-url)))
    (with-current-buffer data-buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (let ((start (point))
            (compressed-data))
        (set-buffer-multibyte nil)
        (goto-char start)
        (setq compressed-data (buffer-substring-no-properties (point) (point-max)))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert compressed-data)
          (condition-case err
              (progn
                (zlib-decompress-region (point-min) (point-max))
                (set-buffer-multibyte t)
                (decode-coding-region (point-min) (point-max) 'utf-8)
                (goto-char (point-min))
                (json-read))
            (error
             (message "Decompression failed: %s" err)
             nil)))))))

(defun time-zones--extract-timezones (data)
  "Extract unique timezones from the downloaded DATA, returning a hash table."
  (let ((timezones (make-hash-table :test 'equal)))
    (seq-doseq (timezone time-zones-custom-timezones)
      (puthash (format "%s %s"
                       (or (map-elt timezone 'flag)
                           (time-zones--country-flag (map-elt timezone 'timezone))
                           time-zones--fallback-flag)
                       (or (map-elt timezone 'city)
                           (map-elt timezone 'state)
                           (map-elt timezone 'country)
                           (map-elt timezone 'timezone)))
               `((country . ,(map-elt timezone 'country))
                 (state . ,(map-elt timezone 'state))
                 (city . ,(map-elt timezone 'city))
                 (timezone . ,(time-zones--resolve-timezone (map-elt timezone 'timezone)))
                 (latitude . ,(map-elt timezone 'latitude))
                 (longitude . ,(map-elt timezone 'longitude))
                 (flag . ,(map-elt timezone 'flag)))
               timezones))
    (seq-doseq (country data)
      (seq-doseq (state (map-elt country 'states))
        (seq-doseq (city (map-elt state 'cities))
          (puthash (format "%s %s - %s, %s"
                           (or (time-zones--country-flag (map-elt country 'name))
                               time-zones--fallback-flag)
                           (map-elt country 'name)
                           (map-elt city 'name)
                           (map-elt state 'name))
                   `((country . ,(map-elt country 'name))
                     (state . ,(map-elt state 'name))
                     (city . ,(map-elt city 'name))
                     (timezone . ,(time-zones--resolve-timezone (map-elt city 'timezone)))
                     (latitude . ,(map-elt city 'latitude))
                     (longitude . ,(map-elt city 'longitude)))
                   timezones))))
    timezones))

;; Major mode

(defun time-zones--save-city-list ()
  "Save the city list to file for persistence across sessions."
  (with-temp-file time-zones--city-list-file
    (insert ";;; Saved time-zones city list -*- lexical-binding: t; -*-\n")
    (insert ";; This file is auto-generated. Do not edit manually.\n\n")
    (insert "(setq time-zones--city-list\n")
    (insert "      '")
    (prin1 time-zones--city-list (current-buffer))
    (insert ")\n")))

(defun time-zones--load-city-list ()
  "Load the city list from file if it exists."
  (when (file-exists-p time-zones--city-list-file)
    (condition-case err
        (load time-zones--city-list-file nil t)
      (error
       (message "Failed to load time-zones city list: %s" err)))))

(defvar time-zones-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") 'time-zones-add-city)
    (define-key map (kbd "D") 'time-zones-delete-city-at-point)
    (define-key map (kbd "r") 'time-zones-refresh)
    (define-key map (kbd "g") 'time-zones-refresh)
    (define-key map (kbd "f") 'time-zones-time-forward)
    (define-key map (kbd "b") 'time-zones-time-backward)
    (define-key map (kbd "F") 'time-zones-time-forward-hour)
    (define-key map (kbd "B") 'time-zones-time-backward-hour)
    (define-key map (kbd "j") 'time-zones-jump-to-date)
    (define-key map (kbd "(") 'time-zones-toggle-showing-details)
    (define-key map (kbd "?") 'time-zones-toggle-showing-help)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "q") 'kill-buffer-and-window)
    map)
  "Keymap for `time-zones-mode'.")

(define-derived-mode time-zones-mode special-mode "time zones"
  "Major mode for displaying and managing timezones.

\\{time-zones-mode-map}"
  (time-zones--load-city-list)
  (time-zones--start-timer)
  (add-hook 'kill-buffer-hook 'time-zones--stop-timer nil t)
  (time-zones--refresh-display))

(defun time-zones--start-timer ()
  "Start the auto-refresh timer."
  (when time-zones--refresh-timer
    (cancel-timer time-zones--refresh-timer))
  (setq time-zones--time-offset 0)
  (setq time-zones--refresh-timer
        (run-at-time
         0 30 (lambda ()
                (when-let* ((buffer (get-buffer "*time zones*"))
                            (_ (buffer-live-p buffer)))
                  (with-current-buffer buffer
                    (time-zones--refresh-display)))))))

(defun time-zones--stop-timer ()
  "Stop the auto-refresh timer."
  (when time-zones--refresh-timer
    (cancel-timer time-zones--refresh-timer)
    (setq time-zones--refresh-timer nil)))

(defun time-zones--round-to-15-minutes (time)
  "Truncate TIME to 15-minute interval (:00, :15, :30, :45)."
  (let* ((decoded (decode-time time))
         (minute (nth 1 decoded))
         (rounded-minute (* 15 (/ minute 15))))
    (setf (nth 1 decoded) rounded-minute)
    (setf (nth 0 decoded) 0)  ;; Zero out seconds
    (apply #'encode-time decoded)))

(defun time-zones--get-display-time ()
  "Get the time to display, accounting for manual offset."
  (let ((base-time (time-add (current-time) time-zones--time-offset)))
    (if (zerop time-zones--time-offset)
        base-time
      (time-zones--round-to-15-minutes base-time))))

(defun time-zones--sort-cities-by-time (cities time)
  "Sort CITIES in relation to TIME.
Returns a new list sorted chronologically, accounting for date changes."
  (sort (copy-sequence cities)
        (lambda (city1 city2)
          (let* ((time1 (string-to-number (format-time-string "%Y%m%d%H%M" time (map-elt city1 'timezone))))
                 (time2 (string-to-number (format-time-string "%Y%m%d%H%M" time (map-elt city2 'timezone)))))
            (< time1 time2)))))

(defun time-zones--format-utc-offset (time timezone)
  "Format UTC offset for TIMEZONE at TIME as a string like `UTC-6' or `UTC+5:30'.
TIME is the time to check the offset for (to handle DST correctly).
TIMEZONE is the timezone string (IANA or POSIX format)."
  (let* ((offset-string (format-time-string "%z" time timezone))
         ;; offset-string is like "-0600" or "+0530"
         (sign (substring offset-string 0 1))
         (hours (string-to-number (substring offset-string 1 3)))
         (minutes (string-to-number (substring offset-string 3 5))))
    (if (zerop minutes)
        (format "UTC%s%d" sign hours)
      (format "UTC%s%d:%02d" sign hours minutes))))

(defun time-zones--is-dst (time timezone)
  "Check if DST is active for TIMEZONE at TIME.
Returns \"DST\" if daylight saving time is in effect, empty string otherwise.
TIME is the time to check.
TIMEZONE is the timezone string (IANA or POSIX format)."
  (let* ((decoded (decode-time time timezone))
         (dst-flag (nth 7 decoded)))
    (eq dst-flag t)))

(defun time-zones--format-city (city local-time max-location-width max-date-width max-offset-width)
  "Format CITY for display.

Consider LOCAL-TIME, MAX-LOCATION-WIDTH, MAX-DATE-WIDTH, and MAX-OFFSET-WIDTH."
  (propertize
   (format (format " %%s %%s  %%s  %%-%ds  %%-%ds  %%-%ds %%s\n" max-location-width max-date-width max-offset-width)
           (if (or (< (string-to-number (format-time-string "%H" local-time (map-elt city 'timezone)))
                      (car time-zones-waking-hours))
                   (>= (string-to-number (format-time-string "%H" local-time (map-elt city 'timezone)))
                       (cdr time-zones-waking-hours)))
               "☽" " ")
           (format-time-string "%H:%M" local-time (map-elt city 'timezone))
           (or (map-elt city 'flag)
               (time-zones--country-flag (map-elt city 'country))
               time-zones--fallback-flag)
           (propertize (or (map-elt city 'city)
                           (map-elt city 'state)
                           (map-elt city 'timezone))
                       'face 'font-lock-builtin-face)
           (format-time-string "%A %d %B" local-time (map-elt city 'timezone))
           (if time-zones-show-details
               (propertize (time-zones--format-utc-offset local-time (map-elt city 'timezone))
                           'face 'shadow)
             "")
           (if time-zones-show-details
               (propertize (if (time-zones--is-dst local-time (map-elt city 'timezone))
                               "★"
                             " ")
                           'face 'shadow)
             ""))
   'time-zones-timezone city))

(defun time-zones--refresh-display ()
  "Refresh the display of cities and their current times."
  (let* ((inhibit-read-only t)
         (current-line (or (line-number-at-pos) 1))
         (local-time (time-zones--get-display-time))
         (title (concat "\n "
                        (propertize (format-time-string "%H:%M %A %d %B" local-time)
                                    'face '(:height 1.5))
                        (propertize (cond
                                     ((zerop time-zones--time-offset) "")
                                     ((> time-zones--time-offset 0) " (future)")
                                     (t " (past)"))
                                    'face '(header-line (:height 1.5)))
                        "\n\n")))
    (erase-buffer)
    (setq header-line-format
          (when time-zones-show-help
            (concat
             "  "
             (propertize "+" 'face 'help-key-binding)
             " add city  "
             (propertize "D" 'face 'help-key-binding)
             " delete city  "
             (propertize "(" 'face 'help-key-binding)
             " details  "
             (propertize "?" 'face 'help-key-binding)
             " help")))
    (insert title)
    (when time-zones--city-list
      (let* ((sorted-cities (time-zones--sort-cities-by-time
                             time-zones--city-list local-time))
             (max-location-width
              (apply #'max
                     (mapcar (lambda (city)
                               (let* ((city-name (map-elt city 'city))
                                      (state (map-elt city 'state))
                                      (timezone (map-elt city 'timezone))
                                      (location (or city-name state timezone)))
                                 (length location)))
                             sorted-cities)))
             (max-date-width
              (apply #'max
                     (mapcar (lambda (city)
                               (length (format-time-string "%A %d %B" local-time (map-elt city 'timezone))))
                             sorted-cities)))
             (max-offset-width
              (apply #'max
                     (mapcar (lambda (city)
                               (length (time-zones--format-utc-offset local-time (map-elt city 'timezone))))
                             sorted-cities))))
        (dolist (city sorted-cities)
          (insert (time-zones--format-city city local-time max-location-width max-date-width max-offset-width)))))
    (unless (seq-empty-p time-zones--city-list)
      (insert "\n"))
    (if time-zones-show-help
        (progn
          (insert (concat
                   " "
                   (propertize "f" 'face 'help-key-binding)
                   (propertize " forward  " 'face 'header-line)
                   (propertize "b" 'face 'help-key-binding)
                   (propertize " backward  " 'face 'header-line)
                   (propertize "j" 'face 'help-key-binding)
                   (propertize " jump  " 'face 'header-line)
                   (propertize "g" 'face 'help-key-binding)
                   (propertize " refresh  " 'face 'header-line)
                   (propertize "q" 'face 'help-key-binding)
                   (propertize " quit  " 'face 'header-line)))
          (insert "\n\n ")))
    (insert "\n\n\n ")
    (when-let ((window (get-buffer-window "*time zones*" t))
               (height (count-lines (point-min) (point-max))))
      (fit-window-to-buffer window height height))
    (goto-char (point-min))
    (when (> current-line 1)
      (forward-line (1- current-line)))))

(defun time-zones--resolve-timezone (iana-tz)
  "Convert IANA-TZ to a format suitable for the current platform.
On Windows, converts to POSIX TZ format by downloading mappings from
https://github.com/nayarsystems/posix_tz_db.  On other platforms,
returns IANA-TZ as-is."
  (if (eq system-type 'windows-nt)
      (let ((mappings (or time-zones--posix-tz-cache
                          (setq time-zones--posix-tz-cache
                                (time-zones--fetch-posix-tz-mappings)))))
        (or (map-elt mappings (intern iana-tz))
            iana-tz))  ; Fallback to IANA if no mapping found
    iana-tz))

(defun time-zones--country-flag (country-name)
  "Get the flag emoji for COUNTRY-NAME."
  (map-elt '(("Afghanistan" . "🇦🇫")
             ("Albania" . "🇦🇱")
             ("Algeria" . "🇩🇿")
             ("Andorra" . "🇦🇩")
             ("Angola" . "🇦🇴")
             ("Argentina" . "🇦🇷")
             ("Armenia" . "🇦🇲")
             ("Australia" . "🇦🇺")
             ("Austria" . "🇦🇹")
             ("Azerbaijan" . "🇦🇿")
             ("Bahrain" . "🇧🇭")
             ("Bangladesh" . "🇧🇩")
             ("Belarus" . "🇧🇾")
             ("Belgium" . "🇧🇪")
             ("Belize" . "🇧🇿")
             ("Benin" . "🇧🇯")
             ("Bhutan" . "🇧🇹")
             ("Bolivia" . "🇧🇴")
             ("Bosnia and Herzegovina" . "🇧🇦")
             ("Botswana" . "🇧🇼")
             ("Brazil" . "🇧🇷")
             ("Brunei" . "🇧🇳")
             ("Bulgaria" . "🇧🇬")
             ("Burkina Faso" . "🇧🇫")
             ("Burundi" . "🇧🇮")
             ("Cambodia" . "🇰🇭")
             ("Cameroon" . "🇨🇲")
             ("Canada" . "🇨🇦")
             ("Central African Republic" . "🇨🇫")
             ("Chad" . "🇹🇩")
             ("Chile" . "🇨🇱")
             ("China" . "🇨🇳")
             ("Colombia" . "🇨🇴")
             ("Comoros" . "🇰🇲")
             ("Costa Rica" . "🇨🇷")
             ("Croatia" . "🇭🇷")
             ("Cuba" . "🇨🇺")
             ("Cyprus" . "🇨🇾")
             ("Czech Republic" . "🇨🇿")
             ("Denmark" . "🇩🇰")
             ("Djibouti" . "🇩🇯")
             ("Dominican Republic" . "🇩🇴")
             ("Ecuador" . "🇪🇨")
             ("Egypt" . "🇪🇬")
             ("El Salvador" . "🇸🇻")
             ("Estonia" . "🇪🇪")
             ("Eswatini" . "🇸🇿")
             ("Ethiopia" . "🇪🇹")
             ("Finland" . "🇫🇮")
             ("France" . "🇫🇷")
             ("Gabon" . "🇬🇦")
             ("Georgia" . "🇬🇪")
             ("Germany" . "🇩🇪")
             ("Ghana" . "🇬🇭")
             ("Greece" . "🇬🇷")
             ("Guatemala" . "🇬🇹")
             ("Guinea" . "🇬🇳")
             ("Guinea-Bissau" . "🇬🇼")
             ("Guyana" . "🇬🇾")
             ("Haiti" . "🇭🇹")
             ("Honduras" . "🇭🇳")
             ("Hungary" . "🇭🇺")
             ("Iceland" . "🇮🇸")
             ("India" . "🇮🇳")
             ("Indonesia" . "🇮🇩")
             ("Iran" . "🇮🇷")
             ("Iraq" . "🇮🇶")
             ("Ireland" . "🇮🇪")
             ("Israel" . "🇮🇱")
             ("Italy" . "🇮🇹")
             ("Jamaica" . "🇯🇲")
             ("Japan" . "🇯🇵")
             ("Jordan" . "🇯🇴")
             ("Kazakhstan" . "🇰🇿")
             ("Kenya" . "🇰🇪")
             ("Kuwait" . "🇰🇼")
             ("Kyrgyzstan" . "🇰🇬")
             ("Laos" . "🇱🇦")
             ("Latvia" . "🇱🇻")
             ("Lebanon" . "🇱🇧")
             ("Lesotho" . "🇱🇸")
             ("Liberia" . "🇱🇷")
             ("Libya" . "🇱🇾")
             ("Liechtenstein" . "🇱🇮")
             ("Lithuania" . "🇱🇹")
             ("Luxembourg" . "🇱🇺")
             ("Madagascar" . "🇲🇬")
             ("Malawi" . "🇲🇼")
             ("Malaysia" . "🇲🇾")
             ("Maldives" . "🇲🇻")
             ("Mali" . "🇲🇱")
             ("Malta" . "🇲🇹")
             ("Mauritania" . "🇲🇷")
             ("Mauritius" . "🇲🇺")
             ("Mexico" . "🇲🇽")
             ("Moldova" . "🇲🇩")
             ("Monaco" . "🇲🇨")
             ("Mongolia" . "🇲🇳")
             ("Montenegro" . "🇲🇪")
             ("Morocco" . "🇲🇦")
             ("Mozambique" . "🇲🇿")
             ("Myanmar" . "🇲🇲")
             ("Namibia" . "🇳🇦")
             ("Nepal" . "🇳🇵")
             ("Netherlands" . "🇳🇱")
             ("New Zealand" . "🇳🇿")
             ("Nicaragua" . "🇳🇮")
             ("Niger" . "🇳🇪")
             ("Nigeria" . "🇳🇬")
             ("North Korea" . "🇰🇵")
             ("North Macedonia" . "🇲🇰")
             ("Norway" . "🇳🇴")
             ("Oman" . "🇴🇲")
             ("Pakistan" . "🇵🇰")
             ("Panama" . "🇵🇦")
             ("Paraguay" . "🇵🇾")
             ("Peru" . "🇵🇪")
             ("Philippines" . "🇵🇭")
             ("Poland" . "🇵🇱")
             ("Portugal" . "🇵🇹")
             ("Qatar" . "🇶🇦")
             ("Romania" . "🇷🇴")
             ("Russia" . "🇷🇺")
             ("Rwanda" . "🇷🇼")
             ("Saudi Arabia" . "🇸🇦")
             ("Senegal" . "🇸🇳")
             ("Serbia" . "🇷🇸")
             ("Singapore" . "🇸🇬")
             ("Slovakia" . "🇸🇰")
             ("Slovenia" . "🇸🇮")
             ("Somalia" . "🇸🇴")
             ("South Africa" . "🇿🇦")
             ("South Korea" . "🇰🇷")
             ("South Sudan" . "🇸🇸")
             ("Spain" . "🇪🇸")
             ("Sri Lanka" . "🇱🇰")
             ("Sudan" . "🇸🇩")
             ("Suriname" . "🇸🇷")
             ("Sweden" . "🇸🇪")
             ("Switzerland" . "🇨🇭")
             ("Syria" . "🇸🇾")
             ("Taiwan" . "🇹🇼")
             ("Tajikistan" . "🇹🇯")
             ("Tanzania" . "🇹🇿")
             ("Thailand" . "🇹🇭")
             ("Togo" . "🇹🇬")
             ("Tunisia" . "🇹🇳")
             ("Turkey" . "🇹🇷")
             ("Turkmenistan" . "🇹🇲")
             ("Uganda" . "🇺🇬")
             ("Ukraine" . "🇺🇦")
             ("United Arab Emirates" . "🇦🇪")
             ("United Kingdom" . "🇬🇧")
             ("United States" . "🇺🇸")
             ("Uruguay" . "🇺🇾")
             ("Uzbekistan" . "🇺🇿")
             ("Venezuela" . "🇻🇪")
             ("Vietnam" . "🇻🇳")
             ("Yemen" . "🇾🇪")
             ("Zambia" . "🇿🇲")
             ("Zimbabwe" . "🇿🇼"))
           country-name))

(provide 'time-zones)

;;; time-zones.el ends here
