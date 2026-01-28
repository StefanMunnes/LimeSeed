Avoid lots of manual click-around in the LimeSurvey web interface by simply creating and editing a user-friendly YAML file. This package converts that structured input so it can be imported directly into LimeSurvey to create a new survey. With this file-oriented approach, you have all the structure, content, and changes in your coding workflow.

Use a structured YAML file as a survey source to import into LimeSurvey


Pipeline -> from lime seed containing all information encapsulated to full grown LimeSurvey ready to harvest


why can it be helpful to preload a yaml file and add it as a separate list to the function:
  - you could create the whole part manually or with code or more useful, have a YAML file with placeholders that you can change to more complex content with code in the script and then assigning the final list object to the function


### seed
1. path to single file containing all (necessary) elements (settings, structure)
2. path to folder containing all the (necessary) files (named accordingly)
3. named list containing paths to files for (necessary) elements
4. named list containing paths to files and (pre-loaded or created) elements as list
5. (pre-loaded or created) list with all (necessary) elements