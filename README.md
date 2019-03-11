
<!-- README.md is generated from README.Rmd. Please edit that file -->
MobileTrigger
=============

The MobileTrigger package allows you to access to R Models, Scripts, and Reports when you only have a mobile device with e-mail capabilities on hand. Here is a visual summary:

<img src="https://cdn.r-bar.net/PID1025/ProcessFlow.gif" alt="Mobile Device R data process flow" style="display: block; margin-left: auto; margin-right: auto; width: 95%">

Installation
============

-   cran: install.package("MobileTrigger") (Coming Soon)
-   github: devtools::install\_github("kenithgrey/MobileTrigger")

Basic Process Flow
------------------

Below is an overview. More extensive instructions can be found at <https://r-bar.net/MobileTriggers/>.

1.  Install the Package
2.  Define a Trigger root path (E.g., C:)
3.  Run the following R script to setup the folder structure. You will need to change the quoted items to fit your situation.

<pre>
SetupWindowsTrigger(path="c:/Triggers",
                    Mail.To = "Your.Email@mobile.com",
                    Mail.From = "R.Triggers@desktop.com",
                    SMTP.Server = "smtp.server.com",
                    SMTP.Port = 123,
                    SMTP.User = "R.Triggers@desktop.com",
                    SMTP.Password = "1234Password"
)
</pre>
1.  Locate the starterMessages.bat file in the Trigger root path. This will launch the starterMessages.R file in headless mode. After running the batch file, 6 Messages with different subjects will be sent to the target e-mail box in step 3. Use these to setup your e-mail rules.

2.  Setup E-mail rules:

<!-- -->

1.  Outlook: You will need to make a small windows registry adjustment to allow OUTLOOK rules to run VBA script. The VBA scripts you need are located in the OUTLOOK.txt file in the Trigger root folder. They are very simple. They either write a message to txt or run an R script in headless mode. When you have the VBA in. Connect each starter messages from step 4 with the appropriate VBA function. Click here for more detailed instructions
2.  ThunderBird: You will need to download the FiltaQuila plugin for ThunderBird. Once installed, adjust the FiltaQuilla settings to allow saving email messages to text files. The text files will need to be saved in the Trigger root folder. Click here for more detailed instructions.

<!-- -->

1.  If the rules are setup correctly when you send an e-mail from the FROM e-mail account in step 3, you will depending on the SUBJECT line:

<!-- -->

1.  List out the available Models, Scripts, or Reports
2.  Run the available Models, Scripts, or Reports.

Again, more detailed instructions can be found at <https://r-bar.net/MobileTriggers/>
