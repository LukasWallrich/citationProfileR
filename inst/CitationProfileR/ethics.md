### Our Background

We would like to acknowledge that we are data science college students at a liberal arts college in Massachusetts. We come from various cultural and ethnic backgrounds (Latin America, Asia, Europe, USA), with two of the team members being international students. All of us identify as women/non-binary individuals, so our team does not have male cisgender members.

We do not have any direct experience working, researching, or teaching in academia for living. We have not experienced the issues posed by discriminating citations or gender bias while working academia or higher educational institutions.

### Ethical Concerns of the Project

1.  The tool relies heavily on multiple third party APIs to ensure functionality. We explored the potential APIs and decided to use the ones that performed the best. However, there is a possibility that CitationProfileR might inherit biases and assumptions made by the creators of the APIs.

1.  One of the APIs that we rely on is Gender-API which guesses genders of individuals based on their first name. Gender-API unfortunately is limited in its ability to accurately predict people's gender identities. It does not account for non-binary/transgender individuals as the API only is limited to guessing female and male genders. So, there is a high chance that the gender guessed by the API is inaccurate. This issue can be harmful for the LGBTQIA+ community especially, who have been historically underrepresented and misrepresented in academia.

1.  The Gender-API has a limited range of names it can accurately guess. The API works best with "Western" names or names from English speaking countries. It is not as accurate with non-Western names. This is also contributing to the bias in CitationProfileR. To at least partially target these issues, we implemented predicted gender accuracy thresholds. If the accuracy of the prediction is below 85%, we display the guessed gender as "inconclusive". In addition to that, the user will be able to download and manually edit any mistakes they find in the diversity report. This will make it possible to counter some of the issues with misrepresentation in CitationProfileR. Though, we acknowledge that these actions are also limited and do not resolve the systematic biases that we inherit from the third party APIs.
