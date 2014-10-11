fooddice
========

Help me I am hungry and don't know what to eat!

Your stomache growls, tells you "I am hungry! Go get and hunt anything", but
you are sitting in front of your computer screen head in revolutionary code
and dont want to waste energy thinking about what or where to eat now?
Dont panic. Relax. Just let Emacs help you.

To install this package put it somewhere in your `load-path' and add
(require 'fooddice) to your ~/.emacs file.

For better results set or add to the following lists

(setq fooddice/meals '(pizza pommes burger noodles bread soup chinese sushi))

(setq fooddice/drinks '(cola water coffee tea beer whisky milk cacao icetea energydrink))

(setq fooddice/restaurants '(italian chinese thai american japanese))

Now ask Emacs what to to

M-x what-to-eat

M-x where-to-eat

M-x what-to-drink
