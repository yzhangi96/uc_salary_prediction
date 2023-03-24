from scholarly import scholarly
import pandas as pd

# local imports
import constants


class ExtractGoogleScholar:
    def __init__(self, name: str):
        """
        Attributes:
            name (str): faculty name to query
        """
        self.name = name

    def query_author(self):
        """
        todo
        """
        search_query = scholarly.search_author(self.name)
        author = next(search_query)
        # if len(author) > 1:
        #     print("Duplicate Warning")
        #     print(author)
        info_all = scholarly.fill(author, sections=constants.SECTION)
        pub_years = [str(x.get("bib").get('pub_year')) for x in info_all.get("publications")]
        pub_years = [y for y in pub_years if y != "None"]

        info_dict = {
            "name": self.name,
            "affiliation": info_all.get("affiliation"),
            "cited_by": info_all.get("citedby"),
            "h_index": info_all.get("hindex"),
            "count_unique_coauthor": len(info_all.get("coauthors")),
            "count_pub": len(info_all.get("publications")),
            "year_first_pub": min(pub_years),
            "year_last_pub": max(pub_years),
        }

        return info_dict


if __name__ == "__main__":

    names_df = pd.read_csv("/Users/ivyzhang/Desktop/Stats305B/final_proj/salary_uc.csv")
    names_df = names_df.replace({"name": constants.MANUAL})
    info = []
    missing = []
    for names in names_df["name"]:
        try:
            extract_scholar = ExtractGoogleScholar(names)
            info.append(extract_scholar.query_author())
            print("Done with ", names)
        except:
            # print("Missing ", names)
            missing.append(names)
            continue

    print(missing)
    info = pd.DataFrame(info)
    final_df = names_df.merge(info, on="name", how="outer")
    final_df.to_csv("/Users/ivyzhang/Desktop/Stats305B/final_proj/google_scholar_uc_faculty.csv")

    # extract_scholar = ExtractGoogleScholar("Michael Tsiang, Lecturer, UCLA")
    # info = pd.DataFrame([extract_scholar.query_author()])
    # info.to_csv("/Users/ivyzhang/Desktop/Stats305B/final_proj/additional18.csv")

