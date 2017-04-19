import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux';

import LetterView from '../../../../view/right-sidebar/filters/abc-filter/letter';
import ArtistContainer from './artist';
import { fetchArtistByLetter } from '../../../../actions';
import { getArtistsByLetter } from '../../../../selectors';


class LetterContainer extends Component {
    constructor(props) {
        super(props);
        this.state = {
            folded: true,
        };
    }

    handleUnfoldLetter = (event) => {
        console.info('handleUnfoldLetter');
        event.preventDefault();
        event.stopPropagation();
        this.setState({ folded: !this.state.folded });
        if (!this.props.artists) {
            this.props.fetchArtistByLetter();
        }
    }

    get artistList() {
        if (this.state.folded) return null;

        const list = this.props.filterOptions.map(artistObj =>
            <ArtistContainer
                id     = {artistObj.id}
                key    = {artistObj.id}
                letter = {artistObj.name}
            />
        );
        return (
            <ul>
                { list }
            </ul>
        );
    }

    render() {
        return (
            <LetterView
                letter  = {this.props.letter}
                onClick = {this.handleUnfoldLetter}
            >
                {this.artistList}
            </LetterView>
        );
    }
}


LetterContainer.propTypes = {
    fetchArtistByLetter: PropTypes.func.isRequired,
    letter             : PropTypes.string.isRequired,
};

const mapStateToProps = (state, props) => ({
    artists: getArtistsByLetter(state, props),
});

const mapDispatchToProps = (dispatch, ownProps) => ({
    fetchArtistByLetter: ()    => dispatch(fetchArtistByLetter(ownProps.id)),
});

export default connect(mapStateToProps, mapDispatchToProps)(LetterContainer);
