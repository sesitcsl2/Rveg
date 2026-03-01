# Rveg 0.1.8

# Rveg 0.1.8 

**This release focuses on ensuring systemic stability and comprehensive documentation following the structural transitions of version 0.1.7.**

## System Stability and Dependency Optimization
The package core logic has been refactored to prioritize base R `stats` and `utils` dependencies, reducing external overhead and ensuring long-term maintainability. Integration with the `xml2` library has been established to facilitate the import of TURBOVEG datasets via the XML schema. Furthermore, internal utility logic has been centralized within `RvegUtils.R` to standardize shared computational tasks across the package.

## Functional Enhancements to Data Transformation
The format transfer suite—comprising `RvegToJuice`, `RvegToTv`, and `TvToRveg` has been updated to ensure full compatibility with the metadata-inclusive database format. These functions now provide more robust data exchange protocols for phytosociological analysis software.

## Refocusing of Database Management Tools
The operational scope of `RvegCheck` has been redefined. While previously utilized for resolving nomenclature code conflicts, it now serves as a database management utility. It allows users to modify project-level attributes, such as titles and descriptions, and in future updates will facilitate the repair of structural inconsistencies and other attributes, like Header fields and Used checklist. Existing functions `RvegLoad` and `RvegCombine` were updated to parse the new metadata headers while maintaining their original utility in data ingestion and table construction.

## Formal Documentation Standards
A complete overhaul of the package documentation has been executed. Every exported function now includes CRAN-compliant help pages with detailed `@details` sections. These sections provide shorter and more up-to-date function description compared to Rveg manual (`vignette(“Rveg”)`). They include formal specifications for the mathematical algorithms utilized in `RvegCombine` and the deterministic string-processing rules employed for `ShortName` generation in `CreateChecklist`.

---

# Rveg 0.1.7 
**Version 0.1.7 introduces a standardized metadata framework designed to ensure data integrity and facilitate easier and broader application.**

## Metadata Architecture and Data Integrity
Both Relevé and Header files now utilize a standardized metadata block, denoted by the `#` prefix. This architecture enables the persistence of project-specific variables, including a unique Database ID that ensures a deterministic link between species data and header attributes. The system automatically logs the `Rveg` version, relevé counts, and temporal metadata (creation and modification timestamps), ensuring compatibility for future schema migrations.

## Dynamic Taxonomic Extension
A significant advancement is the introduction of the "Extra Species" protocol. Users may now define and insert unique taxa or high-precision nomenclature (subspecies, hybrids) directly during the `addReleve()` execution. These records are stored within the project’s local metadata, preserving the integrity of the primary checklist while allowing for project-specific taxonomic flexibility.

## Advances in Checklist Generation
The `CreateChecklist()` function has been expanded to support the direct processing of R character vectors, enabling seamless integration with taxonomic databases via packages such as `rWCVP`. To demonstrate this capability, default checklists for Canada (Quebec) and Portugal have been integrated, alongside a TURBOVEG-compatible checklist for Czechia and Slovakia and checklist for Czech republic by Kaplan et al. (2019).

## User Interface and Operational Efficiency
The `addReleve()` function has been updated with a menu-driven console interface to improve the digitization workflow. This interface provides real-time access to project metadata and includes an integrated help command for on-the-fly documentation access. Additionally, low-level data processing routines were optimized to increase the efficiency of I/O operations for larger-scale phytosociological databases.

## Version Tracking and Documentation
Starting with version 0.1.7, all package developments are formally documented in the NEWS.md file. Furthermore, the database metadata now captures the active Rveg version at the time of creation, ensuring robust backward compatibility and facilitating schema migrations in future releases.
