;; ssh into compute canada
(defun open-ssh-beluga ()
  "Open the SSH file 'mrouss@mymachine.ca' using TRAMP."
  (interactive)
  (find-file "/ssh:mrouss@beluga.computecanada.ca:/home/mrouss/projects/def-azouaq/mrouss"))
