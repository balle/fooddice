;;; fooddice.el --- Help me I am hungry and dont know what to eat!

;; Copyright (C) 2014 Bastian Ballmann

;; Author: Bastian Ballmann <balle@codekid.net>
;; Maintainer: Bastian Ballmann <balle@codekid.net>
;; URL: https://github.com/balle/fooddice
;; Created: 11th Octobre 2014
;; Version: 0.1
;; Keywords: tools, games
;; Package-Requires: ()

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Your stomache growls, tells you "I am hungry! Go get and hunt anything", but
;;; you are sitting in front of your computer screen head in revolutionary code
;;; and dont want to waste energy thinking about what or where to eat now?
;;; Dont panic. Relax. Just let Emacs help you.

;; To install this package put it somewhere in your `load-path' and add
;; (require 'fooddice) to your ~/.emacs file

;; For better results set or add to the following lists
;; (setq fooddice/meals '(pizza pommes burger noodles bread soup chinese sushi))
;; (setq fooddice/drinks '(cola water coffee tea beer whisky milk cacao icetea energydrink))
;; (setq fooddice/restaurants '(italian chinese thai american japanese))

;; Now ask Emacs what to to
;; M-x what-to-eat
;; M-x where-to-eat
;; M-x what-to-drink

;;; Code:

(require 'cl-lib)

(defun fooddice/set-custom (variable value)
  "Set the default value of the variable, and clear the random cache"
  (put variable 'food-list nil)
  (set-default variable value))

(defgroup fooddice nil
  "Let Emacs suggest what to eat"
  :group 'games)

(defcustom fooddice/meals
  '("pizza" "pommes" "burger" "noodles" "bread" "soup" "chinese" "sushi")
  "A list of meals"
  :type '(repeat string)
  :set #'fooddice/set-custom
  :group 'fooddice)

(defcustom fooddice/drinks
  '("cola" "water" "coffee" "tea" "beer" "whisky" "milk" "cacao" "ice tea" "energy drink")
  "A list of drinks"
  :type '(repeat string)
  :set #'fooddice/set-custom
  :group 'fooddice)

(defcustom fooddice/restaurants
  '("italian" "chinese" "thai" "american" "japanese")
  "A list of restaurants"
  :type '(repeat string)
  :set #'fooddice/set-custom
  :group 'fooddice)

(defun what-to-eat ()
  "Tell me what to eat"
  (interactive)
  (message "What about %s?" (fooddice/dice 'fooddice/meals)))

(defun where-to-eat ()
  "Decide where I should go eating"
  (interactive)
  (message "What about %s?" (fooddice/dice 'fooddice/restaurants)))

(defun what-to-drink ()
  "What would you drink?"
  (interactive)
  (message "I would drink a %s" (fooddice/dice 'fooddice/drinks)))

(defun fooddice/dice (list-sym)
  (let ((current-list (get list-sym 'food-list)))
    (if current-list
        (put list-sym 'food-list (cdr current-list))
      (setq current-list (sort
                          (cl-copy-list (symbol-value list-sym))
                          (lambda (&rest _) (zerop (random 2)))))
      (put list-sym 'food-list (cdr current-list)))
    (car current-list)))

(provide 'fooddice)
;;; fooddice.el ends here
