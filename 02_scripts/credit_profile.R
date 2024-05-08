credit_profile <- function(){
    list(
        tabPanel(
            title = "Highlights"
        ),
        
        tabPanel(
            title = "Key features"
        ),
        
        tabPanel(
            title = "Functionality",
            div(
                "Data Upload and Customization: Users have the flexibility to upload their own data, ensuring the tool adapts to specific organizational needs. This feature enhances the tool's versatility and makes it applicable to a wide range of credit portfolios"
            ),
            div(
                "Variable Selection: Allows users to select variables based on their preferences and requirements. This tailored approach allows for a more personalized and targeted credit risk analysis, improving the relevance of insights generated."
            ),
            div(
                "Predictive Modeling and Expected Loss Calculations: Leverage advanced predictive models to forecast credit risks accurately. Additionally, the tool facilitates the calculation of expected losses, providing a holistic view of potential financial impacts. "
            ),
            div(
                "User-Friendly Interface: The RShiny dashboard ensures an intuitive and visually appealing experience. Users can effortlessly navigate through panels, making the tool accessible to users with varying levels of expertise. "
            )
        ),
        
        tabPanel(
            title = "Summary",
            p("In conclusion, the Credit Risk Analysis Tool redefines credit risk management by 
              combining traditional scorecards with cutting-edge predictive analytics. 
              Its unique features, such as user data upload and variable selection, make it a versatile and indispensable tool for financial institutions striving for precision and adaptability in credit risk assessment")
        )
    )
}
