# Rveg 0.1.7

Big release increase the accesibility of package and simplify the use in broader area. Also a new level of stability is added within metadata.

## summary

- updated addReleve() interface
- change of the database files structure, now including metadata marked by hashtag symbol `#`
- Inclusion of several checklist created by `CreateChecklist()` for easier broader use. -> Canada (Quebec), Portugal. You can simply recreate checklist for any area with the same process as shown in example at `help(CreateChecklist)`
- Option to add a new species during the writing process of `addReleve()`
- Improved efficiency
- Added a `NEWS.md` file to track changes to the package.


## Metadata

Metadata are inserted at the start of both Releve & Header files and are recognizable by hashtag symbol `#`. They serve multiple purpose such us to distinguish several databases, store informations a recycle them in Rveg functions and ensuring compatibility. In future they might serve as important part for projects sharing.  At this moment they consist of:
- Project name & Project description. These are optional informations to characterize and sort project.
- Number of Releves. 
- Used checklist. Rveg can recognize used checklist and user doesnt need to remember which checklist was used.
- Rveg version. For compatibility purposes, with the future updates, this can be used for `updating` old databases or to track changes in package functions. 
- Extra species. Now there is option to use species that are not part of the original checklist. These extra species are stored here, therefore they are not directly changing the original checklist, but only expand it for certain project.
- Time of creation
- Time of last change
- Database ID. Database ID are randomly generated tag to connect releve and header.

## CreateChecklist()

Function createchecklist now work directly with R object, additionaly to the txt files. Therefore you are able to input vector of characters (species) to create a checklist. This can be easily utilized with use of other sourcing packages like `rWCVP`, that source species lists from databases. This way, we created a few checklists and included them in default Rveg (Quebec: wcvp_que, Portugal: wcvp_por). Small changes with the rules of creating ShortNames.

## Menu interface

QOL update to the addReleve interface. Showing project informations. For command use help in addReleve dialogue.

## Extra species

This function is designed for use in case of unexpected species occurence, or in case of higher taxonomic precision (eg subspecies, hybrids etc.). Therefore it serves as unique taxon addition to the base checklist, not as tool for creating or merging checklist. During digitalization, when you finds out the desired taxon is not part of the checklist, you can `insert` new species by creating original ShortName code and species name, which will be stored in the database metadata, and are not usable outside the current project.
