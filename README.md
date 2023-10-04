# Many Analysts, One Dataset - Primate Sociality

Welcome to our little experiment, and thank you for your interest! For any questions, or if you would like to receive more detailed or simpler instructions, please contact [a.mielke\@qmul.ac.uk](mailto:a.mielke@qmul.ac.uk) .

## Inclusion Criteria

**This study has the following inclusion criteria. Only continue if you think you fulfill these criteria. If you do not fulfill these criteria, please do not spend your time on this.** Because we will offer co-authorship to participants and because we are interested in how certain choices are made within the field, we can **only use data of researchers who have the following characteristics**:

a)  You have previously worked with primates - it is not important here if you collected your own data or used someone else, or in which setting you worked
b)  You have experience manipulating and analysing social data - interaction rates, dominance hierarchies, etc
c)  You are at least at the level of PhD student
d)  You have a verifiable institutional email address - it does not matter which institution or where it is situated, but we need to check your status

If you are unsure whether you fulfill the criteria, you can fill this form: <https://forms.office.com/e/urCgLUyWv0>{:target="_blank"}  

If you would like us to periodically remind you that you had intended to participate in this study, you can also use the form.

## The study

### Rationale

Scientific research often forces researchers to make choices - about how to collect data, process data, analyse data. This is especially true for non-experimental data, because few researchers will receive the training they need to handle it before they actively work with, and often it's learning-by-doing. Sociality research in primatology is a prime example of this - you have a group of monkeys and you want to find out who is the alpha female and who is friends with each other, but your university course did not prepare you for this. Do you collect data using focal follows or scans? Do you include juveniles or not? Which dominance rank index do you use, and for which interaction type? Do you standardise grooming rates or not? This leads into a 'garden of forking paths': two researchers, starting with the same research question, might make different decisions for each of these choices and end up with very different data, processing steps, and statistical models. We cannot currently assess how much this impacts the published literature because many of these choices are built into the way fieldsites collect their data and because few fieldsites see internal replications with new data.

One approach that has been used to understand the risks involved in these many researcher degrees of freedom is called a 'Many Analysts, One Dataset' approach (<https://doi.org/10.1177/2515245917747646>). The basic idea is that as many researchers as possible carry out an analysis using the same basic raw data, but following the protocols that they usually use in their research group. If enough researchers participate, this approach gives us an overview of the most common differences between studies and whether they have an impact on how researchers would describe their study system. The goal is not to determine who does it right or wrong, but to map the different decisions that are available and make future researchers aware of the effects they have on their own study. In this study, we are trying to do just that, with a basic primate sociality dataset. 

### What to expect 
If you participate, you can expect the following: you will be given a basic primate sociality dataset (comprised of individual demographics, observation effort, displacements, and grooming interactions). These data are simulated, but follow the same patterns as many real-world primate data. We would ask you to prepare the data as you would if you were given data on a new, unknown primate species, while keeping track of all the data manipulations you conduct (preferably using the R script template provided, but we can adapt). We will ask you to run statistical analysis to describe the social system you were given. Afterwards, we will ask you to upload your documentation so that we can determine which choices you made along the data pipeline and analysis, and compare your results with those of other participants. We will then ask you to answer some basic questions about the primates you just studied. 

### Co-authorship 
We acknowledge that participation in this study will take time - we estimate that an experienced participant can probably finish all the analyses and documentation in below 2h, but novices might take up to 4h. This study is an important step to ensure that primatological research is reproducible. However, that does not really help anyone. We will therefore co-authorship to any researcher who fulfills the inclusion criteria (see above) and submits **a reproducible documentation** of their analyses before 01/02/2024. This way, researchers benefit directly from participating in this study. *Please remember that, as a co-author, it is in your own interest to take this study seriously and submit usable work.* If you are unsure whether you fulfill the inclusion criteria, please fill in the form (<https://forms.office.com/e/urCgLUyWv0>) and we will get in touch to discuss your case.

## Setting you up for participation

Still there? Still interested? Amazing!

### The data set

Imagine you are approached by a colleague. They have habituated a group of a previously unknown primate species and can individually identify all group members. They observed them for 2,500h over the course of one year, following all 60 adult group members using focal follows. Because we know nothing about this particular species, they collected basic data on dominance interactions (they are certain that higher-ranking individuals displace lower-ranking individuals most of the time) and grooming interactions, which are fairly common. However, this researcher is not able to turn the raw data into analysable data or run statistical analyses. They are asking you for help, to determine how dominance rank and sex influence grooming patterns. You will have to calculate dominance ranks and prepare grooming interactions and use whatever statistical analyses you find appropriate to answer the two following research questions:

a)  What is the impact of sex on grooming interactions in this species?
b)  What is the impact of dominance rank on grooming interactions in this species?

**Please remember that there are no right or wrong approaches - please prepare and analyse data the way you have done in the past and that you find appropriate!**

You can find the data here: . There are four files:
- *demographics.csv* contains the individuals and their sexes;
- *observation_times.csv* contains information about how often each individual was observed, in case this is information you usually integrate in your analyses;
- *displacements.csv* contains data that can be used to calculate dominance ranks (higher-ranking individuals are the Sender of the displacements);
- *grooming.csv* contains data on grooming interactions (grooming is strictly dyadic and one-directional in this species)

There is also a template () that you can use to guide your steps and that will make it easier for us to compare different submissions later. This template can be copied into an R script, for example, as guidelines.

### Your submission

We require a reproducible document to check how you went from raw data to your results - **it is not enough to simply fill out the questionnaire about the study system in the end**.

This reproducible document can take different forms, depending on the software you would usually use to solve a problem of this kind. - R or Python are preferable, because we can simply compare scripts and used packages - If you usually conduct this kind of analysis in click-based software (Excel/SPSS or a combination), you will have to provide us with a detailed description of your choices, in a way that will allow us to start with the raw data, follow your steps, and arrive at the same conclusions

This project is collaborative and we have an interest in including as many researchers and different approaches as possible to accurately represent the state of primatological research, not just that of a subset of the field that are proficient in statistical programming. Therefore, if you want to participate but are not sure how to best document your work, please contact [a.mielke\@qmul.ac.uk](mailto:a.mielke@qmul.ac.uk){.email} and we can come up with a solution. If we notice after your submission that your code is not reproducible, we will contact you to clarify potential choices you made.

When you have finished the assignment, please follow this link to fill out the Participant Information Sheet, learn how to withdraw from the study, provide information about yourself and your experiences, and upload your reproducible document: <https://forms.office.com/e/um1M5LvKgb>
