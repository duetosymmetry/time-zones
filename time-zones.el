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
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

(require 'json)
(require 'map)
(require 'seq)
(require 'url)

;;; Code:

(defun time-zones ()
  "Open or switch to the time zones buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*time zones*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'time-zones-mode)
        (time-zones-mode)))
    (pop-to-buffer buffer)
    (fit-window-to-buffer)))

(defun time-zones-select-timezone ()
  "Download timezone data and allow user to select a timezone using `completing-read'."
  (message "Fetching...")
  ;; Force in case M-x needs to close
  ;; before blocking on download.
  (redisplay)
  (let* ((data (or time-zones--timezones-cache
                   (setq time-zones--timezones-cache (time-zones--fetch-timezones))))
         (timezone-map (time-zones--extract-timezones data))
         (display-list (sort (map-keys timezone-map) #'string<))
         (selected (completing-read "Select timezone: " display-list nil t)))
    (map-elt timezone-map selected)))

(defun time-zones-add-city ()
  "Add a city to the timezone display."
  (interactive)
  (let ((city-data (time-zones-select-timezone)))
    (when city-data
      (push city-data time-zones--city-list)
      (time-zones--save-city-list)
      (time-zones--refresh-display))))

(defun time-zones-remove-city-at-point ()
  "Remove city at point from the display."
  (interactive)
  (let ((city-data (get-text-property (point) 'time-zones-timezone)))
    (when city-data
      (setq time-zones--city-list
            (seq-remove (lambda (city) (equal city city-data)) time-zones--city-list))
      (time-zones--save-city-list)
      (time-zones--refresh-display))))

(defun time-zones-refresh ()
  "Refresh the timezone display and restart live update."
  (interactive)
  (time-zones--start-timer)
  (time-zones--refresh-display))

(defun time-zones-time-forward ()
  "Move time forward by 15 minutes and stop auto-refresh."
  (interactive)
  (time-zones--stop-timer)
  (setq time-zones--time-offset (+ time-zones--time-offset (* 15 60)))
  (time-zones--refresh-display))

(defun time-zones-time-backward ()
  "Move time backward by 15 minutes and stop auto-refresh."
  (interactive)
  (time-zones--stop-timer)
  (setq time-zones--time-offset (- time-zones--time-offset (* 15 60)))
  (time-zones--refresh-display))

(defun time-zones-time-forward-hour ()
  "Move time forward by 1 hour and stop auto-refresh."
  (interactive)
  (time-zones--stop-timer)
  (setq time-zones--time-offset (+ time-zones--time-offset (* 60 60)))
  (time-zones--refresh-display))

(defun time-zones-time-backward-hour ()
  "Move time backward by 1 hour and stop auto-refresh."
  (interactive)
  (time-zones--stop-timer)
  (setq time-zones--time-offset (- time-zones--time-offset (* 60 60)))
  (time-zones--refresh-display))

(defvar time-zones--timezones-url
  "https://raw.githubusercontent.com/dr5hn/countries-states-cities-database/refs/heads/master/json/countries%2Bstates%2Bcities.json.gz"
  "URL for countries, states, and cities database.")

(defvar time-zones--timezones-cache nil
  "Cache for downloaded timezone data.")

(defconst time-zones--fallback-flag "🏴")

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
                     (timezone . ,(map-elt city 'timezone))
                     (latitude . ,(map-elt city 'latitude))
                     (longitude . ,(map-elt city 'longitude)))
                   timezones))))
    timezones))

;; Major mode

(defvar time-zones--city-list nil
  "List of selected cities for display.")

(defvar time-zones--city-list-file
  (expand-file-name ".time-zones.el" user-emacs-directory)
  "File path for persisting the city list across sessions.")

(defvar time-zones--refresh-timer nil
  "Timer for auto-refreshing the display.")

(defvar time-zones--time-offset 0
  "Manual time offset in seconds.  When non-zero, timer is stopped.")

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
    (define-key map (kbd "-") 'time-zones-remove-city-at-point)
    (define-key map (kbd "r") 'time-zones-refresh)
    (define-key map (kbd "g") 'time-zones-refresh)
    (define-key map (kbd "f") 'time-zones-time-forward)
    (define-key map (kbd "b") 'time-zones-time-backward)
    (define-key map (kbd "F") 'time-zones-time-forward-hour)
    (define-key map (kbd "B") 'time-zones-time-backward-hour)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "Keymap for `time-zones-mode'.")

(define-derived-mode time-zones-mode special-mode "time zones"
  "Major mode for displaying and managing timezones.

\\{time-zones-mode-map}"
  (setq header-line-format
        (concat
         "  "
         (propertize "f" 'face 'help-key-binding)
         " forward  "
         (propertize "b" 'face 'help-key-binding)
         " backward  "
         (propertize "q" 'face 'help-key-binding)
         " quit"))
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

(defun time-zones--refresh-display ()
  "Refresh the display of cities and their current times."
  (let* ((inhibit-read-only t)
         (current-line (or (line-number-at-pos) 1))
         (display-time (time-zones--get-display-time))
         (title (concat "\n "
                        (propertize (format-time-string "%H:%M %A %d %B" display-time)
                                    'face '(:height 1.5))
                        (propertize (cond
                                     ((zerop time-zones--time-offset) "")
                                     ((> time-zones--time-offset 0) " (future)")
                                     (t " (past)"))
                                    'face '(header-line (:height 1.5)))
                        "\n\n")))
    (erase-buffer)
    (insert title)
    (when time-zones--city-list
      ;; Calculate max location width for alignment
      (let ((max-location-width
             (apply #'max
                    (mapcar (lambda (city)
                              (let* ((city-name (map-elt city 'city))
                                     (state (map-elt city 'state))
                                     (location (or city-name state)))
                                (length location)))
                            time-zones--city-list))))
        (dolist (city time-zones--city-list)
          (let* ((timezone (map-elt city 'timezone))
                 (flag (or (time-zones--country-flag (map-elt city 'country))
                           time-zones--fallback-flag))
                 (country (map-elt city 'country))
                 (city-name (map-elt city 'city))
                 (state (map-elt city 'state))
                 (time-str (format-time-string "%H:%M" display-time timezone))
                 (date-str (format-time-string "%A %d %B" display-time timezone))
                 (location (or city-name state))
                 (line-start (point)))
            (insert (format (format " %%s  %%s  %%-%ds  %%s\n" max-location-width)
                            time-str flag
                            (propertize location
                                        'face 'font-lock-builtin-face)
                            date-str))
            (put-text-property line-start (point) 'time-zones-timezone city)))))
    (unless (seq-empty-p time-zones--city-list)
      (insert "\n"))
    (insert (concat
             " "
             (propertize "+" 'face 'help-key-binding)
             (propertize " add city  " 'face 'header-line)
             (propertize "-" 'face 'help-key-binding)
             (propertize " remove city  " 'face 'header-line)
             (propertize "g" 'face 'help-key-binding)
             (propertize " refresh  " 'face 'header-line)))
    (fit-window-to-buffer)
    (goto-char (point-min))
    (when (> current-line 1)
      (goto-line current-line))))

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
