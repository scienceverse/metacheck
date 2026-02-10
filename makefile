AUTHORS_FILE := AUTHORS.md
CONTRIBUTORS_FILE := CONTRIBUTORS.md

MIN_AUTHOR_COMMITS := 5
BOT_REGEX := bot|dependabot|github-actions

.PHONY: authors contributors credits

credits: authors contributors

authors:
	@echo "# Authors" > $(AUTHORS_FILE)
	@echo "" >> $(AUTHORS_FILE)
	@echo "Core contributors with substantial intellectual or technical contributions." >> $(AUTHORS_FILE)
	@echo "Automated accounts and bots are excluded." >> $(AUTHORS_FILE)
	@echo "" >> $(AUTHORS_FILE)
	@git shortlog -sne HEAD \
		| grep -Ev '$(BOT_REGEX)' \
		| awk '$$1 >= $(MIN_AUTHOR_COMMITS)' \
		| sed -E 's/^[[:space:]]*[0-9]+[[:space:]]+//' \
		| sed -E 's/ <.*>//' \
		| sed -E '/^Lakens$$/d' \
		| sort -u \
		| sed 's/^/- /' >> $(AUTHORS_FILE)
	@echo "Updated $(AUTHORS_FILE)"

contributors:
	@echo "# Contributors" > $(CONTRIBUTORS_FILE)
	@echo "" >> $(CONTRIBUTORS_FILE)
	@echo "Additional contributors who provided smaller fixes, feedback, or support." >> $(CONTRIBUTORS_FILE)
	@echo "Automated accounts and bots are excluded." >> $(CONTRIBUTORS_FILE)
	@echo "" >> $(CONTRIBUTORS_FILE)
	@git shortlog -sne HEAD \
		| grep -Ev '$(BOT_REGEX)' \
		| awk '$$1 < $(MIN_AUTHOR_COMMITS)' \
		| sed -E 's/^[[:space:]]*[0-9]+[[:space:]]+//' \
		| sed -E 's/ <.*>//' \
		  sed -E '/^debruine$$/d' \
		  sed -E '/^Jakub Werner/d' \
		| sort -u \
		| sed 's/^/- /' >> $(CONTRIBUTORS_FILE)
	@echo "Updated $(CONTRIBUTORS_FILE)"
